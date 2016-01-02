-module(player).
-import(table_sup,[join_or_create/2,last_game/1,leave/1,zole/1,lielais/1,pass/1,save/2,play/2]).
-import(zole,[is_trump/1,seed_rnd/0]).
-export([start/2,init/2]).

init(Name, TableName) ->
    {ok} = admin:login(Name),
    {ok, Pid} = table_sup:join_or_create(TableName, true),
    seed_rnd(),
    loop(Name, Pid, [], []).

start(Name, Table) ->
    spawn(?MODULE, init, [Name, Table]).

choose_card(Cards, []) ->
    lists:nth(random:uniform(length(Cards)), Cards);
choose_card(Cards, OnTable) ->
    {R,S} = C = lists:last(OnTable),
    Trump = is_trump(C),
    AnyT = lists:any(fun zole:is_trump/1, Cards),
    AnyS = lists:any(fun({_,X}=T) -> X==S andalso not(is_trump(T)) end, Cards),
    F = case {Trump, AnyT, AnyS} of
	{true, true, _} ->
	    lists:filter(fun zole:is_trump/1, Cards);
	{false, _, true} ->
	    lists:filter(fun({_,X}=T) -> X==S andalso not(is_trump(T)) end, Cards);
	_ ->
	    Cards
	end,
    choose_card(F, []).

loop(Name, Table, Cards, OnTable) ->
    receive
	{cards, NewCards} ->
	    loop(Name, Table, NewCards, OnTable);
	{prompt, play} ->
	    C = choose_card(Cards, OnTable),
	    play(Table, C),
	    loop(Name, Table, Cards, OnTable);
	{prompt, save} ->
	    S = lists:sublist(Cards, random:uniform(length(Cards) - 2), 2),
	    save(Table, S),
	    loop(Name, Table, Cards, []);
	{prompt, {choose, _N}} ->
	    R = random:uniform(5),
	    case R of
		L when L<3 -> lielais(Table);
		3 -> zole(Table);
		_ -> pass(Table)
	    end,
	    loop(Name, Table, Cards, []);
	{plays, _, NewOnTable} ->
	    loop(Name, Table, Cards, NewOnTable);
	{end_of_game, _GameNum, _Saved, _GameType, _Taken, _Points} ->
	    loop(Name, Table, Cards, []);
	{wins, _Winner, _CardsTaken} ->
    	    loop(Name, Table, Cards, []);
	{game_type, _T, _N} ->
    	    loop(Name, Table, Cards, OnTable);
	{last_game, _P} ->
    	    loop(Name, Table, Cards, OnTable);
	{table_closed, _TableName, _Pts} ->
    	    admin:logout()
    end.
