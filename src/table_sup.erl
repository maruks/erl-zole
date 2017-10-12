%%%-------------------------------------------------------------------
%% @doc table fsm supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(table_sup).

-behaviour(supervisor).

-export([start_link/0,init/1,join_or_create/2,close_table/1,last_game/1,leave/1,zole/1,lielais/1,pass/1,save/2,play/2,create_table/1,disconnect/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 3600}, [{table_fsm, {table_fsm, start_link, []}, transient, 2000, worker, [table_fsm]}]} }.

% API

join_or_create(TableName, Create) ->
    R = admin:get_or_create_table(TableName, Create),
    case R of
	{ok, Pid, PlayerName} ->
	    do_join(Pid, PlayerName);
	E ->
	    E
    end.

create_table(TableName) ->
    supervisor:start_child(?MODULE, [TableName]).

close_table(TablePid) when is_pid(TablePid) ->
    supervisor:terminate_child(?MODULE, TablePid).

last_game(TablePid) when is_pid(TablePid) ->
    gen_statem:call(TablePid, {last_game}).

leave(TablePid) when is_pid(TablePid) ->
    gen_statem:call(TablePid, {leave}).

disconnect(Table) ->
    gen_statem:call(Table, {disconnect}).

zole(TablePid) when is_pid(TablePid) ->
    gen_statem:call(TablePid, {zole}).

lielais(TablePid) when is_pid(TablePid) ->
    gen_statem:call(TablePid, {lielais}).

pass(TablePid) when is_pid(TablePid) ->
    gen_statem:call(TablePid, {pass}).

save(TablePid, Cards) when is_pid(TablePid) ->
    gen_statem:call(TablePid, {save, Cards}).

play(TablePid, Card) when is_pid(TablePid) ->
    gen_statem:call(TablePid, {play, Card}).

% internal functions

do_join(Table, Name) ->
    R = gen_statem:call(Table, {join, Name}),
    case R of
	{ok} ->
	    {ok, Table};
	E -> E
    end.
