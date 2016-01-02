-module(test_player).
-include_lib("stdlib/include/assert.hrl").
-import(table_sup,[join_or_create/2,last_game/1,leave/1,zole/1,lielais/1,pass/1,save/2,play/2]).
-import(lists,[foldl/3,any/2,last/1,filter/2,nth/2,sublist/3,flatten/1]).
-import(maps,[values/1]).
-export([init/3,start/1,start_1/2,start_3/1,enable_log/0]).

-define(POINTS_PER_GAME, 120).
-define(TRICKS_PER_GAME, 8).
-define(CARDS_WON_PER_GAME, 24).
-define(PLAYERS, 3).

init(Name, TableName, Games2Play) ->
    {ok} = admin:login(Name),
    {ok, Pid} = table_sup:join_or_create(TableName, true),
    zole:seed_rnd(),
    loop(Name, Pid, [], [], Games2Play).

choose_card(Cards, []) ->
    nth(random:uniform(length(Cards)), Cards);
choose_card(Cards, OnTable) ->
    {_,S} = C = last(OnTable),
    Trump = zole:is_trump(C),
    AnyT = any(fun zole:is_trump/1, Cards),
    AnyS = any(fun({_,X}=T) -> X==S andalso not(zole:is_trump(T)) end, Cards),
    F = case {Trump, AnyT, AnyS} of
	{true, true, _} ->
	    filter(fun zole:is_trump/1, Cards);
	{false, _, true} ->
	    filter(fun({_,X}=T) -> X==S andalso not(zole:is_trump(T)) end, Cards);
	_ ->
	    Cards
	end,
    choose_card(F, []).

loop(Name, Table, Cards, OnTable, Games2Play) ->
    receive
	{cards, NewCards} ->
	    lager:debug("Player ~p Cards ~p~n",[Name, NewCards]),
	    loop(Name, Table, NewCards, OnTable, Games2Play);
	{prompt, play} ->
	    C = choose_card(Cards, OnTable),
    	    lager:debug("Player ~p plays ~p ~p ~n",[Name , C, OnTable]),
	    play(Table, C),
	    loop(Name, Table, Cards, OnTable, Games2Play);
	{prompt, save} ->
	    S = sublist(Cards, random:uniform(length(Cards) - 2), 2),
	    save(Table, S),
       	    lager:debug("Player ~p saves ~p~n",[Name, S]),
	    loop(Name, Table, Cards, [], Games2Play);
	{prompt, {choose, _N}} ->
	    R = random:uniform(5),
	    case R of
		L when L<3 -> lielais(Table);
		3 -> zole(Table);
		_ -> pass(Table)
	    end,
	    lager:debug("Player ~p chooses ~p  ~n",[Name, R]),
	    loop(Name, Table, Cards, [], Games2Play);
	{plays, P, NewOnTable} ->
	    lager:debug("~p ~p PLAYS ~p ~p ~n",[Name, P, hd(NewOnTable), NewOnTable]),
	    loop(Name, Table, Cards, NewOnTable, Games2Play);
	{end_of_game, GameNum, Saved, GameType, Taken, {Score, Points, TotalPoints} = Pts} ->
    	    lager:debug("END ~p ~p ~p ~p ~p ~p ~n",[Name, GameNum, Saved, GameType, Taken, Pts]),
	    {TricksWon, PointsWon} = foldl(fun({T,P},{TA, PA}) -> {TA + T, PA + P} end, {0,0}, values(Score)),
	    SavedPoints = foldl(fun({R,_},A) -> A + zole:points(R) end, 0, Saved),
	    ExpPoints = case GameType of
			    {lielais, _} -> PointsWon;
			    _ -> PointsWon + SavedPoints
			end,
	    ?assertEqual(?TRICKS_PER_GAME, TricksWon),
	    ?assertEqual(?POINTS_PER_GAME, ExpPoints),
	    ?assertEqual(?CARDS_WON_PER_GAME, length(flatten(values(Taken)))),
	    ?assertEqual(0, foldl(fun erlang:'+'/2, 0, values(Points))),
	    ?assertEqual(0, foldl(fun erlang:'+'/2, 0, values(TotalPoints))),
   	    ?assertEqual(?PLAYERS, maps:size(Taken)),
       	    ?assertEqual(?PLAYERS, maps:size(Points)),
       	    ?assertEqual(?PLAYERS, maps:size(TotalPoints)),
       	    ?assertEqual(?PLAYERS, maps:size(Score)),
	    loop(Name, Table, Cards, [], Games2Play);
	{wins, Winner, CardsTaken} ->
	    lager:debug("~p ~p WINS ~p~n",[Name, Winner, CardsTaken]),
    	    loop(Name, Table, Cards, [], Games2Play);
	{game_type, T, Games2Play} ->
	    lager:debug("~p GAME TYPE ~p ~p ~n",[Name, T, Games2Play]),
	    last_game(Table),
    	    loop(Name, Table, Cards, OnTable, Games2Play);
	{game_type, T, N} ->
	    lager:debug("~p GAME TYPE ~p ~p ~n",[Name, T, N]),
    	    loop(Name, Table, Cards, OnTable, Games2Play);
	{last_game, P} ->
	    lager:debug("~p ~p PLAYS LAST GAME ~n",[Name, P]),
    	    loop(Name, Table, Cards, OnTable, Games2Play);
	{table_closed, TableName, {GamesPlayed,Pts}} ->
    	    lager:debug("~p TABLE CLOSED ~p ~p~n",[Name, TableName, Pts]),
	    ?assertEqual(Games2Play, GamesPlayed),
	    ?assertEqual(?PLAYERS, maps:size(Pts)),
    	    ?assertEqual(0, foldl(fun erlang:'+'/2, 0, values(Pts))),
	    io:format("Test finished ~p~n",[Pts]),
	    admin:logout();
	M ->
	    lager:error("MSG ~p ~p~n",[Name, M]),
	    loop(Name, Table, Cards, OnTable, Games2Play)
    end.

enable_log() ->
    lager:set_loglevel(lager_file_backend, "zole-console.log", debug).

% play against bots
start(Games2Play) when is_integer(Games2Play), Games2Play > 0 ->
    T = integer_to_list(erlang:unique_integer([positive])),
    P = integer_to_list(erlang:unique_integer([positive])),
    spawn(?MODULE, init, ["player-test-" ++ P, "pvb-table-" ++ T, Games2Play]).

% start single player
start_1(TableName, Games2Play) when is_integer(Games2Play), Games2Play > 0 ->
    P = integer_to_list(erlang:unique_integer([positive])),
    spawn(?MODULE, init, ["player-test-" ++ P, TableName, Games2Play]).

% start 3 players
start_3(Games2Play) when is_integer(Games2Play), Games2Play > 0 ->
    T = integer_to_list(erlang:unique_integer([positive])),
    P1 = integer_to_list(erlang:unique_integer([positive])),
    P2 = integer_to_list(erlang:unique_integer([positive])),
    P3 = integer_to_list(erlang:unique_integer([positive])),
    spawn(?MODULE, init, ["player-test-" ++ P1, "table-test-" ++ T , Games2Play]),
    spawn(?MODULE, init, ["player-test-" ++ P2, "table-test-" ++ T , Games2Play]),
    spawn(?MODULE, init, ["player-test-" ++ P3, "table-test-" ++ T , Games2Play]).
