-module(test_player).
-include_lib("stdlib/include/assert.hrl").
-import(table_sup,[join_or_create/2,last_game/1,leave/1,zole/1,lielais/1,pass/1,save/2,play/2]).
-import(lists,[foldl/3,any/2,last/1,filter/2,nth/2,sublist/3,flatten/1,all/2]).
-import(maps,[values/1]).
-export([init/4,start/1,start_1/2,start_3/1,enable_log/0,run_tests/0]).

-define(POINTS_PER_GAME, 120).
-define(TRICKS_PER_GAME, 8).
-define(CARDS_WON_PER_GAME, 24).
-define(PLAYERS, 3).

init(Name, TableName, Games2Play, Observer) ->
    {ok} = admin:login(Name),
    admin:subscribe(self()),
    {ok, Pid} = table_sup:join_or_create(TableName, true),
    zole:seed_rnd(),
    loop(Name, Pid, [], [], Games2Play, Observer).

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

loop(Name, Table, Cards, OnTable, Games2Play, Observer) ->
    receive
	{cards, NewCards} ->
	    lager:debug("Player ~p Cards ~p ~p~n",[Name,length(NewCards),NewCards]),
	    loop(Name, Table, NewCards, OnTable, Games2Play, Observer);
	{prompt, play} ->
	    C = choose_card(Cards, OnTable),
    	    lager:debug("Player ~p plays ~p ~p ~n",[Name , C, OnTable]),
	    play(Table, C),
	    loop(Name, Table, Cards, OnTable, Games2Play, Observer);
	{prompt, save} ->
	    S = sublist(Cards, random:uniform(length(Cards) - 2), 2),
	    save(Table, S),
       	    lager:debug("Player ~p saves ~p~n",[Name, S]),
	    loop(Name, Table, Cards, [], Games2Play, Observer);
	{prompt, {choose, N}} ->
	    R = random:uniform(5),
	    case R of
		L when L<3 -> lielais(Table);
		3 -> zole(Table);
		_ -> pass(Table)
	    end,
	    lager:debug("Player ~p chooses ~p ~p  ~n",[Name, N, R]),
	    loop(Name, Table, Cards, [], Games2Play, Observer);
	{plays, P, NewOnTable} ->
	    lager:debug("Player ~p ~p PLAYS ~p ~p ~n",[Name, P, hd(NewOnTable), NewOnTable]),
	    loop(Name, Table, Cards, NewOnTable, Games2Play, Observer);
	{end_of_game, GameNum, Saved, GameType, Taken, {Score, Points, TotalPoints} = Pts} ->
    	    lager:debug("END Player ~p ~p ~p ~p ~p ~p ~n",[Name, GameNum, Saved, GameType, Taken, Pts]),
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
	    loop(Name, Table, Cards, [], Games2Play, Observer);
	{wins, Winner, CardsTaken} ->
	    lager:debug("Player ~p ~p WINS ~p~n",[Name, Winner, CardsTaken]),
    	    loop(Name, Table, Cards, [], Games2Play, Observer);
	{game_type, T, Games2Play} ->
	    lager:debug("Player ~p GAME TYPE ~p ~p ~n",[Name, T, Games2Play]),
	    last_game(Table),
    	    loop(Name, Table, Cards, OnTable, Games2Play, Observer);
	{players, Players} ->
	    lager:debug("Player ~p Players ~p~n",[Name, Players]),
	    ?assert(all(fun(OtherPlayersName) -> Name =/= OtherPlayersName end, Players)),
	    ?assertEqual(2, length(Players)),
    	    loop(Name, Table, Cards, OnTable, Games2Play, Observer);
	{game_type, T, N} ->
	    lager:debug("Player ~p GAME TYPE ~p ~p ~n",[Name, T, N]),
    	    loop(Name, Table, Cards, OnTable, Games2Play, Observer);
	{last_game, P} ->
	    lager:debug("Player ~p ~p PLAYS LAST GAME ~n",[Name, P]),
    	    loop(Name, Table, Cards, OnTable, Games2Play, Observer);
	{open_tables, Tables} ->
	    lager:debug("Player ~p OPEN TABLES ~p~n",[Name, Tables]),
	    ?assert(is_map(Tables)),
	    admin:unsubscribe(self()),
	    loop(Name, Table, Cards, OnTable, Games2Play, Observer);
	{table_closed, TableName, {GamesPlayed,Pts}} ->
    	    lager:debug("Player ~p TABLE CLOSED ~p ~p~n",[Name, TableName, Pts]),
	    ?assertEqual(Games2Play, GamesPlayed),
	    ?assertEqual(?PLAYERS, maps:size(Pts)),
    	    ?assertEqual(0, foldl(fun erlang:'+'/2, 0, values(Pts))),
	    admin:logout(),
	    io:format("Test finished ~p~n",[Pts]),
	    case is_pid(Observer) of
		true -> Observer ! test_finished;
		false -> ok
	    end;
	M ->
	    lager:error("Unknown message ~p ~p~n",[Name, M]),
	    ?assert(false)
    end.

enable_log() ->
    lager:set_loglevel(lager_file_backend, "zole-console.log", debug).

% play against bots
start(Games2Play) when is_integer(Games2Play), Games2Play > 0 ->
    T = integer_to_list(erlang:unique_integer([positive])),
    P = integer_to_list(erlang:unique_integer([positive])),
    spawn(?MODULE, init, ["player-test-" ++ P, "pvb-table-" ++ T, Games2Play, self()]).

% start single player
start_1(TableName, Games2Play) when is_integer(Games2Play), Games2Play > 0 ->
    P = integer_to_list(erlang:unique_integer([positive])),
    spawn(?MODULE, init, ["player-test-" ++ P, TableName, Games2Play, self()]).

% start 3 players
start_3(Games2Play) when is_integer(Games2Play), Games2Play > 0 ->
    T = integer_to_list(erlang:unique_integer([positive])),
    P1 = integer_to_list(erlang:unique_integer([positive])),
    P2 = integer_to_list(erlang:unique_integer([positive])),
    P3 = integer_to_list(erlang:unique_integer([positive])),
    spawn(?MODULE, init, ["player-test-" ++ P1, "table-test-" ++ T , Games2Play, ok]),
    spawn(?MODULE, init, ["player-test-" ++ P2, "table-test-" ++ T , Games2Play, ok]),
    spawn(?MODULE, init, ["player-test-" ++ P3, "table-test-" ++ T , Games2Play, self()]).

% run tests
run_tests() ->
    application:ensure_all_started(zole),
    enable_log(),
    start_3(1),
    receive
	test_finished -> erlang:halt()
    end.
