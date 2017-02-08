-module(table_fsm).
-define(NUMBER_OF_PLAYERS, 3).
-behaviour(gen_fsm).
-import(lists,[delete/2,nth/2,map/2,nth/2,zip/2,member/2,reverse/1,foreach/2]).
-import(proplists,[get_value/2,is_defined/2]).
-import(maps,[get/2,put/3,from_list/1,to_list/1]).
-import(zole,[deal_cards/0,sort_cards/1,winner/1,is_legal_play/3,is_legal_save/2]).
-export([init/1,start_link/1,terminate/3,handle_info/3,code_change/4,handle_sync_event/4,handle_event/3]).
-export([wait2join/3,wait2choose/3,wait2save/3,playing/3,table_closed/3]).

-type card():: {atom() | pos_integer(), atom()}.
-type player_cards() :: #{pid() => list(card())}.
-type players() :: list({pid(), string()}).
-type game_type() :: {zole, pid()} | {lielais, pid()} | {galds}.

-record(state,{name :: nonempty_string(),
	       players = [] :: players(),
	       cards :: undefined | player_cards(),
	       saved :: undefined | list(card()),
	       type :: undefined | game_type(),
	       points :: undefined | #{pid() => integer()},
	       num :: undefined | pos_integer(),
	       last :: undefined | boolean(),
	       hand :: undefined | pos_integer(),
	       remn :: undefined | integer(),
	       table :: undefined | list({pid(), card()}) | list(card()),
	       taken :: undefined | player_cards()}).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, Name, []).

init(Name) ->
    process_flag(trap_exit, true),
    {ok, wait2join, #state{name = Name}}.

% FSM states

wait2join({join, Name}, {From, _}, #state{name = TableName, players = Players} = S) ->
    case is_defined(From, Players) of
	true ->
	    illegal_message(wait2join, S);
	false ->
	    NextPlayers = Players ++ [{From, Name}],
	    if length(NextPlayers) == ?NUMBER_OF_PLAYERS ->
		    admin:table_unavailable(TableName),
		    Points = from_list(zip(player_pids(NextPlayers), [0,0,0])),
		    [{Player1, Player1Name}, {Player2, Player2Name}, {Player3, Player3Name}] = NextPlayers,
		    Player1 ! {players, [Player2Name, Player3Name]},
		    Player2 ! {players, [Player3Name, Player1Name]},
		    Player3 ! {players, [Player1Name, Player2Name]},
		    new_game(S#state{players = NextPlayers, points = Points, num = 1, last = false});
	       true ->
		    admin:table_available(TableName, player_names(NextPlayers)),
		    {reply, {ok}, wait2join, S#state{players = NextPlayers}}
	    end
	end;
wait2join({leave}, {From, _}, #state{name = TableName, players = Players} = S) ->
    case is_defined(From, Players) of
	true ->
	    NewPlayers = proplists:delete(From, Players),
	    case length(NewPlayers) of
		0 -> admin:table_finished(TableName),
		    {reply, {ok}, table_closed, []};
		_ -> admin:table_available(TableName, player_names(NewPlayers)),
		    {reply, {ok}, wait2join, S#state{players = NewPlayers}}
		end;
	false ->
	    illegal_message(wait2join, S)
	end;
wait2join(_, _, S) ->
    illegal_message(wait2join, S).

wait2choose(Msg, {Player, _}, #state{players= Players, cards = CardsMap, table = Table, hand = Hand} = S) ->
    Pids = player_pids(Players),
    case member(Player, Pids) andalso nth(Hand, Pids) == Player of
	true ->
	    case Msg of
		{zole} ->
		    start_playing(S#state{type = {zole, Player}, saved = Table});
		{lielais} ->
		    NewCards = get(Player, CardsMap) ++ Table,
		    NextCards = CardsMap#{Player := NewCards},
		    send_cards(Player, NewCards),
		    send_prompt(Player, save),
		    {reply, {ok}, wait2save, S#state{type = {lielais, Player}, cards = NextCards, saved = []}};
		 {pass} ->
		    pass(S);
		 _ ->
		    illegal_message(wait2choose, S)
	    end;
       false ->
	    illegal_message(wait2choose, S)
    end;
wait2choose(_, _, S) ->
    illegal_message(wait2choose, S).

wait2save({save, Cards}, {Player, _}, #state{players = Players, cards = CardsMap, type = {lielais, Player}} = S) when is_list(Cards) ->
    Pids = player_pids(Players),
    case member(Player, Pids) andalso is_legal_save(Cards, get(Player, CardsMap)) of
	false ->
	    illegal_message(wait2save, S);
	true ->
	    NewCards = get(Player, CardsMap) -- Cards,
	    NextCards = CardsMap#{Player := NewCards},
	    send_cards(Player, get(Player, NextCards)),
	    start_playing(S#state{cards = NextCards, saved = Cards, type = {lielais, Player}})
    end;
wait2save(_, _, S) ->
    illegal_message(wait2save, S).

playing({play, Card}, {From, _}, #state{players = Players, cards = CardsMap, hand =  Hand, table = Table} = S) when is_tuple(Card) ->
    Pids = player_pids(Players),
    case nth(Hand,Pids) == From andalso is_legal_play(Card, get(From, CardsMap), table_cards(Table)) of
	true ->
	    play(Card, From, S);
	false ->
	    illegal_message(playing, S)
	end;
playing(_ , _,  S) ->
    illegal_message(playing, S).

table_closed(_, _, S) ->
    illegal_message(table_closed, S).

% internal functions

play(Card, Player, #state{players = Players, table = Table, cards = Cards, hand = Hand, taken = Taken} = S) ->
    NextTable = [{Player, Card} | Table],
    CardsLeft = delete(Card, get(Player, Cards)),
    NextCards = Cards#{Player := CardsLeft},
    send_cards(Player, CardsLeft),
    send_to_all(Players, {plays, get_value(Player, Players), table_cards(NextTable)}),
    case length(NextTable) of
	?NUMBER_OF_PLAYERS ->
	    CardsPlayed = table_cards(NextTable),
	    WinnerCard = winner(reverse(CardsPlayed)),
	    Winner = get_winner(WinnerCard, NextTable),
	    send_to_all(Players, {wins, get_value(Winner, Players), CardsPlayed}),
	    NextTaken = Taken#{Winner := [CardsPlayed | get(Winner, Taken)]},
	    case length(CardsLeft) of
		0 ->
		    finish_game(S#state{taken = NextTaken});
		_ ->
		    NextHand = index_of(fun({P,_N}) -> P == Winner end, Players),
	            send_prompt(NextHand, Players, play),
		    {reply, {ok}, playing, S#state{cards = NextCards, hand = NextHand, table = [], taken = NextTaken}}
	    end;
	_ ->
	    NextHand = 1 + Hand rem ?NUMBER_OF_PLAYERS,
	    send_prompt(NextHand, Players, play),
	    {reply, {ok}, playing, S#state{cards = NextCards, hand = NextHand, table = NextTable}}
    end.

index_of(Pred,[H | T]) ->
    case Pred(H) of
	true ->
	    1;
	false ->
	    1 + index_of(Pred, T)
    end.

map_map(Map, Fn) ->
    maps:fold(fun(K,V,A)-> {NK, NV} = Fn(K,V), A#{NK => NV} end, maps:new(), Map).

merge_with(Fn, M1, M2) ->
    maps:fold(fun(K,V,A)-> A#{K := Fn(get(K, A), V)} end, M1, M2).

finish_game(#state{players = Players, saved = Saved, type = GameType, taken = Taken, points = TotalPoints, num = GameNum} = S) ->
    MapFn = fun(K,V) -> {get_value(K, Players), V} end,
    {Score, Points} = zole:result_points(GameType , Saved, Taken),
    NewTotalPoints = merge_with(fun erlang:'+'/2, TotalPoints, Points),
    GtMsg = game_type(GameType, Players),
    PointsMsg = {map_map(Score, MapFn), map_map(Points, MapFn), map_map(NewTotalPoints, MapFn)},
    TakenMsg = map_map(Taken, MapFn),
    send_to_all(Players, {end_of_game, GameNum, Saved, GtMsg, TakenMsg, PointsMsg}),
    new_game(S#state{points = NewTotalPoints, num = GameNum + 1}).

new_game(#state{name = TableName, players = Players, points = TotalPoints, num = GameNum, last = true} ) ->
    MapFn = fun(K,V) -> {get_value(K, Players), V} end,
    send_to_all(Players, {table_closed, TableName, {GameNum - 1, map_map(TotalPoints, MapFn)}}),
    admin:table_finished(TableName),
    {reply, {ok}, table_closed, []};
new_game(#state{players = Players, num = GameNum, last = false} = S) ->
    {C1, C2, C3, T} = deal_cards(),
    Cards = [C1, C2, C3],
    CardsMap = from_list(zip(player_pids(Players), Cards)),
    send_cards(CardsMap),
    Hand = first_hand(GameNum),
    Rem = 2,
    send_prompt(Hand, Players, {choose, Rem}),
    {reply, {ok}, wait2choose, S#state{cards = CardsMap, table = T, hand = Hand, remn = Rem}}.

player_pids(Players) ->
    map(fun({Pid,_})-> Pid end, Players).

player_names(Players) ->
    map(fun({_,Name})-> Name end, Players).

first_hand(GameNum) ->
    1 + (GameNum - 1) rem ?NUMBER_OF_PLAYERS.

pass(#state{remn = 0, table = Table} = S) ->
    start_playing(S#state{type = {galds}, saved = Table});
pass(#state{players = Players, hand = Hand, remn = Rem} = S) ->
    NextHand = 1 + Hand rem ?NUMBER_OF_PLAYERS,
    send_prompt(NextHand, Players, {choose, Rem-1}),
    {reply, {ok}, wait2choose, S#state{hand =  NextHand, remn = Rem - 1}}.

start_playing(#state{players = Players, type = GameType, num = GameNum} = S) ->
    Hand = first_hand(GameNum),
    send_prompt(Hand, Players, play),
    send_to_all(Players, {game_type, game_type(GameType, Players), GameNum}),
    {reply, {ok}, playing, S#state{table = [], hand = Hand, taken = from_list(zip(player_pids(Players), [[],[],[]]))}}.

game_type({T, Pid}, Players) ->
    {T, get_value(Pid, Players)};
game_type(T, _Players) ->
    T.

table_cards(Table) ->
    map(fun({_Player,Card}) -> Card end,Table).

get_winner(Card,[ {Player, Card} | _R]) ->
    Player;
get_winner(Card,[_P | R]) ->
    get_winner(Card, R).

send_prompt(Player, Msg) ->
    Player ! {prompt, Msg}.

send_prompt(Num, Players, Msg) ->
    {Player,_} = nth(Num, Players),
    send_prompt(Player, Msg).

send_to_all(Players, Message) ->
    foreach(fun({P,_}) -> P ! Message end, Players).

send_cards(Player, Cards) ->
    Player ! {cards, sort_cards(Cards)}.

send_cards(CardsMap) when is_map(CardsMap) ->
    send_cards(to_list(CardsMap));
send_cards([{P, C} | Tl]) ->
    send_cards(P, C),
    send_cards(Tl);
send_cards([]) ->
    ok.

sync_event({last_game}, {From, _} , StateName, #state{players = Players} = S) ->
    {Reply, NewStateData} = case {StateName, S} of
				{wait2save, #state{last = false}} ->
				    {{ok}, S#state{last = true}};
				{wait2choose, #state{last = false}} ->
		    		    {{ok}, S#state{last = true}};
				{playing, #state{last = false}} ->
				    {{ok}, S#state{last = true}};
				_ ->
				    {{error, illegal_message}, S}
			    end,
    case Reply of
	{ok} ->
	    Pids = player_pids(Players),
	    case member(From, Pids) of
		false ->
		    {reply, {error, illegal_message}, StateName, S};
		true ->
		    send_to_all(Players, {last_game, get_value(From, Players)}),
		    {reply, Reply, StateName, NewStateData}
	    end;
	_ ->
	    {reply, Reply, StateName, S}
    end;
sync_event({disconnect}, {_, _} , table_closed, StateData) ->
    illegal_message(table_closed, StateData);
sync_event({disconnect}, {From, R} , State, #state{name = TableName, players = Players, points = TotalPoints, num = GameNum} = StateData) ->
    case proplists:is_defined(From, Players) of
	true ->
	    case State of
		wait2join ->
		    wait2join({leave}, {From, R}, StateData);
		_ ->
		    send_to_all(Players, {disconnected, proplists:get_value(From, Players)}),
		    MapFn = fun(K,V) -> {proplists:get_value(K, Players), V} end,
		    NewPoints = put(From, get(From, TotalPoints) - 16, TotalPoints),
		    send_to_all(Players, {table_closed, TableName, {GameNum, map_map(NewPoints, MapFn)}}),
		    admin:table_finished(TableName),
		    {reply, {ok}, table_closed, []}
	    end;
	false ->
	    illegal_message(State, StateData)
    end.

illegal_message(State, Data) ->
    {reply,{error, illegal_message}, State, Data}.

% callbacks

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(Event, From, StateName, StateData) ->
    sync_event(Event, From, StateName, StateData).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _StateData) ->
    ok.
