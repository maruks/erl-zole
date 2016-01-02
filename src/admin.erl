-module(admin).
-behaviour(gen_server).
-define(PLAY_VERSUS_BOT_TABLE,"pvb-table").
-import(maps,[get/2,put/3,is_key/2,keys/1,get/3,update/3,to_list/1,remove/2]).
-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([list_avail_tables/0,login/1,logout/0,table_finished/1,table_available/2,table_unavailable/1,get_or_create_table/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, {maps:new(), maps:new(), maps:new()}}.

% API

login(PlayerName) ->
    gen_server:call(?MODULE, {login, PlayerName}).

logout() ->
    gen_server:call(?MODULE, {logout}).

list_avail_tables() ->
    gen_server:call(?MODULE, {list_tables}).

get_or_create_table(Name, Create) ->
    gen_server:call(?MODULE, {get_or_create_table, Name, Create}).

table_finished(Name) ->
    gen_server:cast(?MODULE, {table_finished, Name}).

table_available(Name, Players) ->
    gen_server:cast(?MODULE, {table_available, Name, Players}).

table_unavailable(Name) ->
    gen_server:cast(?MODULE, {table_unavailable, Name}).

% internal functions

call({list_tables}, _, {_Tables, _Players, Avail} = S) ->
    {{ok, Avail}, S};
call({login, Name}, {From, _}, {Tables, Players, Avail} = S) ->
    case is_key(From, Players) of
	true ->
	    {{error, already_registered}, S};
	false ->
	    {{ok}, {Tables, put(From, Name, Players), Avail}}
    end;
call({logout}, {From, _}, {Tables, Players, Avail} = S) ->
    case is_key(From, Players) of
	false ->
	    {{error, not_registered}, S};
	true ->
	    {{ok}, {Tables, remove(From, Players), Avail}}
    end;
call({get_or_create_table, TableName, Create}, {From, _}, {Tables, Players, Avail} = S) ->
    case is_key(From, Players) of
	false ->
	    {{error, unknown_pid}, S};
	true ->
	    E = is_key(TableName, Tables),
	    PlayerName = get(From, Players),
	    case {E, Create} of
		{true, _} ->
		    {{ok, get(TableName, Tables), PlayerName}, S};
		{false, false} ->
		    {{error, not_registered}, S};
		{false, true} ->
		    {ok, Pid} = table_sup:create_table(TableName),
		    {{ok, Pid, PlayerName}, {put(TableName, Pid, Tables), Players, Avail}}
	    end
    end.

start_player_bots(_, 0) ->
    ok;
start_player_bots(TableName, HowMany) when is_integer(HowMany), HowMany > 0 ->
    PlayerName = "player-bot-" ++ integer_to_list(erlang:unique_integer([positive])),
    player:start(PlayerName, TableName),
    start_player_bots(TableName, HowMany - 1).

cast({table_unavailable, Name}, {Tables, Players, Avail}) ->
    {Tables, Players, remove(Name, Avail)};
cast({table_available, Name, PlayersAvail}, {Tables, Players, Avail}) ->
    StartBots = length(PlayersAvail) == 0 andalso lists:prefix(?PLAY_VERSUS_BOT_TABLE, Name),
    if
	StartBots -> start_player_bots(Name, 2);
	true -> ok
    end,
    {Tables, Players, put(Name, PlayersAvail, Avail)};
cast({table_finished, Name}, {Tables, Players, Avail}) ->
    Pid = get(Name, Tables),
    table_sup:close_table(Pid),
    {remove(Name, Tables), Players, Avail}.

% callbacks

handle_call(Request, From, State) ->
    {Reply, NewState} = call(Request, From, State),
    {reply, Reply, NewState}.

handle_cast(Request, State) ->
    NewState = cast(Request, State),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
