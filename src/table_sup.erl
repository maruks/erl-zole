%%%-------------------------------------------------------------------
%% @doc table fsm supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(table_sup).

-behaviour(supervisor).

-export([start_link/0,init/1,join_or_create/2,close_table/1,last_game/1,leave/1,zole/1,lielais/1,pass/1,save/2,play/2,create_table/1]).

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

close_table(TablePid) ->
    supervisor:terminate_child(?MODULE, TablePid).

last_game(Table) ->
    gen_fsm:sync_send_all_state_event(Table, {last_game}).

leave(Table) ->
    gen_fsm:sync_send_event(Table, {leave}).

zole(Table) ->
    gen_fsm:sync_send_event(Table, {zole}).

lielais(Table) ->
    gen_fsm:sync_send_event(Table, {lielais}).

pass(Table) ->
    gen_fsm:sync_send_event(Table, {pass}).

save(Table, Cards) ->
    gen_fsm:sync_send_event(Table, {save, Cards}).

play(Table, Card) ->
    gen_fsm:sync_send_event(Table, {play, Card}).

% internal functions

do_join(Table, Name) ->
    R = gen_fsm:sync_send_event(Table, {join, Name}),
    case R of
	{ok} ->
	    {ok, Table};
	E -> E
    end.
