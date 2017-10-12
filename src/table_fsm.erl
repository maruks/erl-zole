-module(table_fsm).
-define(NUMBER_OF_PLAYERS, 3).
-behaviour(gen_statem).
-import(lists,[delete/2,nth/2,map/2,nth/2,zip/2,member/2,reverse/1,foreach/2]).
-import(proplists,[get_value/2,is_defined/2]).
-import(maps,[get/2,put/3,from_list/1,to_list/1]).
-import(zole,[deal_cards/0,sort_cards/1,winner/1,is_legal_play/3,is_legal_save/2]).

-export([init/1,start_link/1,terminate/3,code_change/4,callback_mode/0]).

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

callback_mode() ->
    state_functions.

start_link(Name) ->
    gen_statem:start_link(?MODULE, Name, []).

init(Name) ->
    process_flag(trap_exit, true),
    {ok, wait2join, #state{name = Name}}.

% FSM states

wait2join({call, {Pid, _} = From}, {join, Name}, #state{name = TableName, players = Players} = S) ->
    case is_defined(From, Players) of
	true ->
	    illegal_message(From);
	false ->
	    NextPlayers = Players ++ [{Pid, Name}],
	    if length(NextPlayers) == ?NUMBER_OF_PLAYERS ->
		    admin:table_unavailable(TableName),
		    Points = from_list(zip(player_pids(NextPlayers), [0,0,0])),
		    [{Player1, Player1Name}, {Player2, Player2Name}, {Player3, Player3Name}] = NextPlayers,
		    Player1 ! {players, [Player2Name, Player3Name]},
		    Player2 ! {players, [Player3Name, Player1Name]},
		    Player3 ! {players, [Player1Name, Player2Name]},
		    {NextState, NextData} = new_game(S#state{players = NextPlayers, points = Points, num = 1, last = false}),
		    {next_state, NextState, NextData, {reply, From, {ok}}};
	       true ->
		    admin:table_available(TableName, player_names(NextPlayers)),
		    {keep_state, S#state{players = NextPlayers}, {reply, From, {ok}}}
	    end
	end;
wait2join({call, {Pid, _} = From}, {leave}, #state{name = TableName, players = Players} = S) ->
    case is_defined(Pid, Players) of
	true ->
	    NewPlayers = proplists:delete(Pid, Players),
	    case length(NewPlayers) of
		0 -> admin:table_finished(TableName),
		    {next_state, table_closed, [], {reply, From, {ok}}};
		_ -> admin:table_available(TableName, player_names(NewPlayers)),
		    {keep_state, S#state{players = NewPlayers}, {reply, From, {ok}}}
		end;
	false ->
	    illegal_message(From)
	end;
wait2join({call, From}, Msg, S) ->
    handle_event(Msg, From, wait2join, S).

wait2choose({call, {Pid, _} = From}, Msg, #state{players= Players, cards = CardsMap, table = Table, hand = Hand} = S) ->
    Pids = player_pids(Players),
    case member(Pid, Pids) andalso nth(Hand, Pids) == Pid of
	true ->
	    case Msg of
		{zole} ->
		    {NextState, NextData} = start_playing(S#state{type = {zole, Pid}, saved = Table}),
		    {next_state, NextState, NextData, {reply, From, {ok}}};
		{lielais} ->
		    NewCards = get(Pid, CardsMap) ++ Table,
		    NextCards = CardsMap#{Pid := NewCards},
		    send_cards(Pid, NewCards),
		    send_prompt(Pid, save),
		    {next_state, wait2save, S#state{type = {lielais, Pid}, cards = NextCards, saved = []}, {reply, From, {ok}}};
		 {pass} ->
		    {NextState, NextData} = pass(S),
		    {next_state, NextState, NextData, {reply, From, {ok}}};
		 _ ->
		    illegal_message(From)
	    end;
       false ->
	    illegal_message(From)
    end;
wait2choose({call, From}, Msg, S) ->
    handle_event(Msg, From, wait2choose, S).

wait2save({call, {Pid, _} = From}, {save, Cards}, #state{players = Players, cards = CardsMap, type = {lielais, Pid}} = S) when is_list(Cards) ->
    Pids = player_pids(Players),
    case member(Pid, Pids) andalso is_legal_save(Cards, get(Pid, CardsMap)) of
	false ->
	    illegal_message(From);
	true ->
	    NewCards = get(Pid, CardsMap) -- Cards,
	    NextCards = CardsMap#{Pid := NewCards},
	    send_cards(Pid, get(Pid, NextCards)),
	    {NextState, NextData} = start_playing(S#state{cards = NextCards, saved = Cards, type = {lielais, Pid}}),
	    {next_state, NextState, NextData, {reply, From, {ok}}}
    end;
wait2save({call, From}, Msg, S) ->
    handle_event(Msg, From, wait2save, S).

playing({call, {Pid,_} = From}, {play, Card},  #state{players = Players, cards = CardsMap, hand =  Hand, table = Table} = S) when is_tuple(Card) ->
    Pids = player_pids(Players),
    case nth(Hand,Pids) == Pid andalso is_legal_play(Card, get(Pid, CardsMap), table_cards(Table)) of
	true ->
	    {NextState, NextData} = play(Card, Pid, S),
	    {next_state, NextState, NextData, {reply, From, {ok}}};
	false ->
	    illegal_message(From)
	end;
playing({call, From} , Msg,  S) ->
    handle_event(Msg, From, playing, S).

table_closed({call, From}, _, _) ->
    illegal_message(From).

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
		    {playing, S#state{cards = NextCards, hand = NextHand, table = [], taken = NextTaken}}
	    end;
	_ ->
	    NextHand = 1 + Hand rem ?NUMBER_OF_PLAYERS,
	    send_prompt(NextHand, Players, play),
	    {playing, S#state{cards = NextCards, hand = NextHand, table = NextTable}}
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
    {table_closed, []};
new_game(#state{players = Players, num = GameNum, last = false} = S) ->
    {C1, C2, C3, T} = deal_cards(),
    Cards = [C1, C2, C3],
    CardsMap = from_list(zip(player_pids(Players), Cards)),
    send_cards(CardsMap),
    Hand = first_hand(GameNum),
    Rem = 2,
    send_prompt(Hand, Players, {choose, Rem}),
    {wait2choose, S#state{cards = CardsMap, table = T, hand = Hand, remn = Rem}}.

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
    {wait2choose, S#state{hand =  NextHand, remn = Rem - 1}}.

start_playing(#state{players = Players, type = GameType, num = GameNum} = S) ->
    Hand = first_hand(GameNum),
    send_prompt(Hand, Players, play),
    send_to_all(Players, {game_type, game_type(GameType, Players), GameNum}),
    {playing, S#state{table = [], hand = Hand, taken = from_list(zip(player_pids(Players), [[],[],[]]))}}.

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

handle_event({last_game}, {Pid, _} = From, StateName, #state{players = Players} = S) ->
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
	    case member(Pid, Pids) of
		false ->
		    {keep_state, S, {reply, From, {error, illegal_message} }};
		true ->
		    send_to_all(Players, {last_game, get_value(Pid, Players)}),
		    {keep_state, NewStateData, {reply, From, Reply}}
	    end;
	_ ->
	    {keep_state_and_data, {reply, From, Reply}}
    end;
handle_event({disconnect}, From, table_closed, _) ->
    illegal_message(From);
handle_event({disconnect}, {Pid, _} = From , State, #state{name = TableName, players = Players, points = TotalPoints, num = GameNum} = StateData) ->
    case proplists:is_defined(Pid, Players) of
	true ->
	    case State of
		wait2join ->
		    wait2join({call, From}, {leave}, StateData);
		_ ->
		    send_to_all(Players, {disconnected, proplists:get_value(Pid, Players)}),
		    MapFn = fun(K,V) -> {proplists:get_value(K, Players), V} end,
		    NewPoints = put(Pid, get(Pid, TotalPoints) - 16, TotalPoints),
		    send_to_all(Players, {table_closed, TableName, {GameNum, map_map(NewPoints, MapFn)}}),
		    admin:table_finished(TableName),
		    {next_state, table_closed, [], {reply, From, {ok}}}
	    end;
	false ->
	    illegal_message(From)
    end.

illegal_message(From) ->
    {keep_state_and_data, {reply, From, {error, illegal_message}}}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _StateData) ->
    ok.
