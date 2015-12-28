-module(player).
-compile(export_all).

send(Name, Table, Msg) ->
    R = table_fsm:send_msg(Table, Msg),
    io:format("~p Msg ~p Reply ~p~n", [Name,Msg,R]),
    case {Msg, R} of
	{{play,_}, {error, _}} ->
	    self() ! {prompt, play};
	{{save,_}, {error, _}} ->
	    self() ! {prompt, save};
	_ ->
	    ok
    end.

choice() ->
    [lielais,zole,pass].

print_menu(Xs) ->
    S = lists:seq(1, length(Xs)),
    M = lists:zip(S, Xs),
    lists:foreach(fun({N,E}) -> io:format("~p ~p~n",[N,E]) end, M).

init(Name, Table) ->
    send(Name,Table, {join}),
    loop(Name, Table, []).

loop(Name, Table, Cards) ->
    receive
	{cards, NewCards} ->
	    loop(Name, Table, NewCards);
	{prompt, play} ->
	    io:format("Player ~p~n",[Name]),
	    print_menu(Cards),
	    C = list_to_integer(io:get_chars(">",1)),
	    send(Name,Table, {play, lists:nth(C, Cards)}),
	    loop(Name, Table, Cards);
	{prompt, save} ->
    	    io:format("Player ~p~n",[Name]),
	    print_menu(Cards),
	    Inp = io:get_chars(">",2),
	    Cs = lists:map(fun(N)-> lists:nth(list_to_integer([N]),Cards) end, Inp),
	    send(Name,Table, {save, Cs}),
	    loop(Name, Table, Cards);
	{prompt, choose} ->
    	    io:format("Player ~p Cards ~p~n",[Name, Cards]),
	    print_menu(choice()),
	    C = list_to_integer(io:get_chars(">",1)),
	    send(Name,Table, {lists:nth(C, choice())}),
	    loop(Name, Table, Cards);
	{plays, P, Cards} ->
	    io:format("~p plays ~p ~p ~n",[P,hd(Cards),Cards]),
	    loop(Name, Table, Cards);
	{end_of_game, Saved, GameType, Taken} ->
    	    io:format("END ~p ~p ~p ~n",[Saved, GameType, Taken]),
	    loop(Name, Table, Cards);
	{wins, Winner, Cards} ->
	    io:format("~p wins ~p~n",[Winner, Cards]),
    	    loop(Name, Table, Cards);
	M ->
	    io:format("MSG ~p~n",[M]),
	    loop(Name, Table, Cards)
    end.


start(Name, Table) ->
    spawn(?MODULE, init, [Name, Table]).

foo() ->
    application:start(zole),
    zole_sup:create_table(table),
    P1 = start(player1, table),
    P2 = start(player2, table),
    P3 = start(player3, table),
    {P1, P2, P3}.
