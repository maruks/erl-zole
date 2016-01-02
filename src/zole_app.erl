%%%-------------------------------------------------------------------
%% @doc zole app
%% @end
%%%-------------------------------------------------------------------

-module(zole_app).

-behaviour(application).

-export([start/2,stop/1,start/0]).

start() ->
    application:ensure_all_started(zole).

start(_StartType, _StartArgs) ->
    zole_sup:start_link().

stop(_State) ->
    ok.
