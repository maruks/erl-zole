%%%-------------------------------------------------------------------
%% @doc zole top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zole_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 10, 3600}, [child(admin, worker), child(table_sup, supervisor)]}}.

child(Module, Type) ->
    {Module, {Module, start_link, []}, permanent, 2000, Type, [Module]}.
