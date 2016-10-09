-module(table_fsm).
-define(NUMBER_OF_PLAYERS, 3).
-behaviour(gen_fsm).
-import(lists,[delete/2,nth/2,map/2,nth/2,zip/2,member/2,reverse/1,foreach/2]).
-import(proplists,[get_value/2,is_defined/2]).
-import(maps,[get/2,put/3,from_list/1,to_list/1]).
-import(zole,[deal_cards/0,sort_cards/1,winner/1,is_legal_play/3,is_legal_save/2]).
-export([init/1,start_link/1,terminate/3,handle_info/3,code_change/4,handle_sync_event/4,handle_event/3]).
-export([wait2join/3,wait2choose/3,wait2save/3,playing/3,table_closed/3]).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, Name, []).

init(Name) ->
    process_flag(trap_exit, true),
    {ok, wait2join, {Name, []}}.

% FSM states

wait2join({join, Name}, {From, _}, {TableName, Players} = S) ->
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
		    new_game(TableName, NextPlayers, {Points, 1, false});
	       true ->
		    admin:table_available(TableName, player_names(NextPlayers)),
		    {reply, {ok}, wait2join, {TableName, NextPlayers}}
	    end
	end;
wait2join({leave}, {From, _}, {TableName, Players} = S) ->
    case is_defined(From, Players) of
	true ->
	    NewPlayers = proplists:delete(From, Players),
	    case length(NewPlayers) of
		0 -> admin:table_finished(TableName),
		    {reply, {ok}, table_closed, []};
		_ -> admin:table_available(TableName, player_names(NewPlayers)),
		    {reply, {ok}, wait2join, {TableName, NewPlayers}}
		end;
	false ->
	    illegal_message(wait2join, S)
	end;
wait2join(_, _, S) ->
    illegal_message(wait2join, S).

wait2choose(Msg, {Player, _}, {TableName, Players, CardsMap, Table, PNL, Hand, Rem} = S) ->
    Pids = player_pids(Players),
    case member(Player, Pids) andalso nth(Hand, Pids) == Player of
	true ->
	    case Msg of
		{zole} ->
		    start_playing(TableName, Players, CardsMap, Table, {zole, Player}, PNL);
		{lielais} ->
		    NewCards = get(Player, CardsMap) ++ Table,
		    NextCards = CardsMap#{Player := NewCards},
		    send_cards(Player, NewCards),
		    send_prompt(Player, save),
		    {reply, {ok}, wait2save, {TableName, Players, NextCards, [], {lielais, Player}, PNL}};
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

wait2save({save, Cards}, {Player, _}, {TableName, Players, CardsMap, [], {lielais, Player}, PNL} = S) when is_list(Cards) ->
    Pids = player_pids(Players),
    case member(Player, Pids) andalso is_legal_save(Cards, get(Player, CardsMap)) of
	false ->
	    illegal_message(wait2save, S);
	true ->
	    NewCards = get(Player, CardsMap) -- Cards,
	    NextCards = CardsMap#{Player := NewCards},
	    send_cards(Player, get(Player, NextCards)),
	    start_playing(TableName, Players, NextCards, Cards, {lielais, Player}, PNL)
    end;
wait2save(_, _, S) ->
    illegal_message(wait2save, S).

playing({play, Card}, {From, _}, {_, Players, CardsMap, _, _, _, Hand, Table, _} = S) when is_tuple(Card)->
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

play(Card, Player, {TableName, Players, Cards, Saved, GameType, PNL, Hand, Table, Taken}) ->
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
		    finish_game(TableName, Players, Saved, GameType , NextTaken, PNL);
		_ ->
		    NextHand = index_of(fun({P,_N}) -> P == Winner end, Players),
	            send_prompt(NextHand, Players, play),
		    {reply, {ok}, playing, {TableName, Players, NextCards, Saved, GameType, PNL, NextHand, [], NextTaken}}
	    end;
	_ ->
	    NextHand = 1 + Hand rem ?NUMBER_OF_PLAYERS,
	    send_prompt(NextHand, Players, play),
	    {reply, {ok}, playing, {TableName, Players, NextCards, Saved, GameType, PNL, NextHand, NextTable, Taken}}
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

finish_game(TableName, Players, Saved, GameType, Taken, {TotalPoints, GameNum, LastGame}) ->
    MapFn = fun(K,V) -> {get_value(K, Players), V} end,
    {Score, Points} = zole:result_points(GameType , Saved, Taken),
    NewTotalPoints = merge_with(fun erlang:'+'/2, TotalPoints, Points),
    GtMsg = game_type(GameType, Players),
    PointsMsg = {map_map(Score, MapFn), map_map(Points, MapFn), map_map(NewTotalPoints, MapFn)},
    TakenMsg = map_map(Taken, MapFn),
    send_to_all(Players, {end_of_game, GameNum, Saved, GtMsg, TakenMsg, PointsMsg}),
    new_game(TableName, Players, {NewTotalPoints, GameNum + 1, LastGame}).

new_game(TableName, Players, {TotalPoints, GameNum, true} ) ->
    MapFn = fun(K,V) -> {get_value(K, Players), V} end,
    send_to_all(Players, {table_closed, TableName, {GameNum - 1, map_map(TotalPoints, MapFn)}}),
    admin:table_finished(TableName),
    {reply, {ok}, table_closed, []};
new_game(TableName, Players, {_, GameNum, false} = P) ->
    {C1, C2, C3, T} = deal_cards(),
    Cards = [C1, C2, C3],
    StateData = {TableName, Players, Cards, T, P, first_hand(GameNum), 2},
    {reply, {ok}, wait2choose, start_game(StateData)}.

player_pids(Players) ->
    map(fun({Pid,_})-> Pid end, Players).

player_names(Players) ->
    map(fun({_,Name})-> Name end, Players).

first_hand(GameNum) ->
    1 + (GameNum - 1) rem ?NUMBER_OF_PLAYERS.

start_game({TableName, Players, Cards, Table, PNL , Hand, Rem}) ->
    CardsMap = from_list(zip(player_pids(Players), Cards)),
    send_cards(CardsMap),
    send_prompt(Hand, Players, {choose, Rem}),
    {TableName, Players, CardsMap, Table, PNL , Hand , Rem}.

pass({TableName, Players, CardsMap, Table, PNL, _, 0}) ->
    start_playing(TableName, Players, CardsMap, Table, {galds}, PNL);
pass({TableName, Players, CardsMap, Table, PNL, Hand, Rem}) ->
    NextHand = 1 + Hand rem ?NUMBER_OF_PLAYERS,
    send_prompt(NextHand, Players, {choose, Rem-1}),
    {reply, {ok}, wait2choose, {TableName, Players, CardsMap, Table, PNL, NextHand, Rem-1}}.

start_playing(TableName, Players, CardsMap, Saved, GameType, {_, GameNum, _}=PNL) ->
    Hand = first_hand(GameNum),
    send_prompt(Hand, Players, play),
    send_to_all(Players, {game_type, game_type(GameType, Players), GameNum}),
    {reply, {ok}, playing, {TableName, Players, CardsMap, Saved, GameType, PNL, Hand, [], from_list(zip(player_pids(Players), [[],[],[]]))}}.

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

sync_event({last_game}, {From, _} , StateName, StateData) ->
    {Reply, NewStateData} = case {StateName, StateData} of
				{wait2save, {Table, Players, Cards, [], {lielais, Player}, {P,N,false}}} ->
				    {{ok}, {Table, Players, Cards, [], {lielais, Player}, {P,N,true}}};
				{wait2choose, {Table, Players, Cards, T, {P,N,false}, H, R}} ->
		    		    {{ok}, {Table, Players, Cards, T, {P,N,true}, H, R}};
				{playing, {Table, Players, Cards, S, Type, {P,N,false}, H, T, K}} ->
				    {{ok}, {Table, Players, Cards, S, Type, {P,N,true}, H, T, K}};
				_ ->
				    {{error, illegal_message}, StateData}
			    end,
    case Reply of
	{ok} ->
	    Pls = element(2, NewStateData),
	    Pids = player_pids(Pls),
	    case member(From, Pids) of
		false ->
		    {reply, {error, illegal_message}, StateName, StateData};
		true ->
		    send_to_all(Pls, {last_game, get_value(From, Pls)}),
		    {reply, Reply, StateName, NewStateData}
	    end;
	_ ->
	    {reply, Reply, StateName, StateData}
    end;
sync_event({disconnect}, {_, _} , table_closed, StateData) ->
    illegal_message(table_closed, StateData);
sync_event({disconnect}, {From, R} , State, StateData) ->
    TableName = element(1, StateData),
    Players = element(2, StateData),
    case proplists:is_defined(From, Players) of
	true ->
	    case State of
		wait2join ->
		    wait2join({leave}, {From, R}, {TableName, Players});
		_ ->
		    send_to_all(Players, {disconnected, proplists:get_value(From, Players)}),
		    {TotalPoints, GameNum, _} = pnl(State, StateData),
		    MapFn = fun(K,V) -> {proplists:get_value(K, Players), V} end,
		    NewPoints = put(From, get(From, TotalPoints) - 16, TotalPoints),
		    send_to_all(Players, {table_closed, TableName, {GameNum, map_map(NewPoints, MapFn)}}),
		    admin:table_finished(TableName),
		    {reply, {ok}, table_closed, []}
	    end;
	false ->
	    illegal_message(State, StateData)
    end.

pnl(wait2choose, S) ->
    element(5,S);
pnl(wait2save, S) ->
    element(6, S);
pnl(playing, S) ->
    element(6, S).

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
