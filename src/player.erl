-module(player).
-import(table_sup,[join_or_create/2,last_game/1,leave/1,zole/1,lielais/1,pass/1,save/2,play/2]).
-import(lists,[last/1,any/2,filter/2,nth/2,sublist/3]).
-import(zole,[is_trump/1]).
-export([start/2,init/2]).

-type card():: {atom() | pos_integer(), atom()}.

-record(state,{name :: string(),
	       table :: pid(),
	       cards = [] :: list(card()),
	       on_table = []}).

init(Name, TableName) ->
    {ok} = admin:login(Name),
    {ok, Pid} = table_sup:join_or_create(TableName, true),
    loop(#state{name = Name, table = Pid}).

start(Name, Table) ->
    spawn(?MODULE, init, [Name, Table]).

choose_card(Cards, []) ->
    nth(rand:uniform(length(Cards)), Cards);
choose_card(Cards, OnTable) ->
    {R,S} = C = last(OnTable),
    Trump = is_trump(C),
    AnyT = any(fun zole:is_trump/1, Cards),
    AnyS = any(fun({_, X} = T) -> X==S andalso not(is_trump(T)) end, Cards),
    F = case {Trump, AnyT, AnyS} of
	{true, true, _} ->
	    filter(fun zole:is_trump/1, Cards);
	{false, _, true} ->
	    filter(fun({_,X}=T) -> X==S andalso not(is_trump(T)) end, Cards);
	_ ->
	    Cards
	end,
    choose_card(F, []).

loop(#state{table = Table, cards = Cards, on_table = OnTable} = S) ->
    receive
	{cards, NewCards} ->
	    loop(S#state{cards = NewCards});
	{prompt, play} ->
	    C = choose_card(Cards, OnTable),
	    play(Table, C),
	    loop(S);
	{prompt, save} ->
	    ToSave = sublist(Cards, rand:uniform(length(Cards) - 2), 2),
	    {ok} = save(Table, ToSave),
	    loop(S#state{on_table=[]});
	{prompt, {choose, _N}} ->
	    R = rand:uniform(5),
	    case R of
		L when L<3 -> lielais(Table);
		3 -> zole(Table);
		_ -> pass(Table)
	    end,
	    loop(S#state{on_table=[]});
	{plays, _, NewOnTable} ->
	    loop(S#state{on_table = NewOnTable});
	{end_of_game, _GameNum, _Saved, _GameType, _Taken, _Points} ->
	    loop(S#state{on_table = []});
	{wins, _Winner, _CardsTaken} ->
    	    loop(S#state{on_table = []});
	{table_closed, _TableName, _Pts} ->
    	    admin:logout();
	_ ->
    	    loop(S)
    end.
