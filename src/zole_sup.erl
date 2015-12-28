%%%-------------------------------------------------------------------
%% @doc zole top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zole_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,create_table/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 1, 3600}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

create_table(Name) ->
    ChildSpec = {table_fsm, {table_fsm, start_link, [Name]}, permanent, 2000, worker, [table_fsm]},
    supervisor:start_child(?MODULE, ChildSpec).
