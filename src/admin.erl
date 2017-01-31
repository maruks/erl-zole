-module(admin).
-behaviour(gen_server).
-define(PLAY_VERSUS_BOT_TABLE,"pvb-table").
-import(lists,[member/2,prefix/2,foreach/2]).
-import(maps,[get/2,put/3,is_key/2,keys/1,get/3,update/3,to_list/1,remove/2,values/1,is_key/2,new/0]).
-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([list_avail_tables/0,login/1,logout/0,table_finished/1,table_available/2,table_unavailable/1,get_or_create_table/2,subscribe/1,unsubscribe/1]).

-record(state, {tables = new(), players = new(), avail = new(), subs = sets:new()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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

subscribe(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

% calls

call({list_tables}, _, #state{avail = Avail} = S) ->
    {{ok, Avail}, S};
call({login, Name}, {From, _}, #state{players=Players} = S) ->
    Error = is_key(From, Players) orelse member(Name, values(Players)),
    if Error ->
	    {{error, already_registered}, S};
	true ->
	    {{ok}, S#state{players=Players#{From => Name}}}
    end;
call({logout}, {From, _}, #state{players = Players, subs = Subs} = S) ->
    case is_key(From, Players) of
	false ->
	    {{error, not_registered}, S};
	true ->
	    {{ok}, S#state{players=remove(From, Players),subs=sets:del_element(From, Subs)}}
    end;
call({get_or_create_table, TableName, Create}, {From, _}, #state{tables=Tables, players=Players} = S) ->
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
		    {{ok, Pid, PlayerName}, S#state{ tables=Tables#{TableName => Pid}}}
	    end
    end.

% casts

cast({table_unavailable, Name}, #state{avail = Avail, subs = Subs} = S) ->
    NewAvail = remove(Name, Avail),
    foreach(fun(P) -> P ! {open_tables, NewAvail} end, sets:to_list(Subs)),
    S#state{avail = NewAvail};
cast({table_available, Name, PlayersAvail}, #state{avail = Avail, subs = Subs} = S) ->
    StartBots = length(PlayersAvail) == 1 andalso prefix(?PLAY_VERSUS_BOT_TABLE, Name),
    if
	StartBots -> start_player_bots(Name, 2);
	true -> ok
    end,
    NewAvail = Avail#{Name => PlayersAvail},
    foreach(fun(P) -> P ! {open_tables, NewAvail} end, sets:to_list(Subs)),
    S#state{avail = NewAvail};
cast({table_finished, Name}, #state{tables = Tables, avail = Avail, subs = Subs} = S) ->
    Pid = get(Name, Tables),
    table_sup:close_table(Pid),
    NewAvail = remove(Name, Avail),
    foreach(fun(P) -> P ! {open_tables, NewAvail} end, sets:to_list(Subs)),
    S#state{tables=remove(Name, Tables), avail = NewAvail};
cast({subscribe, Pid}, #state{players = Players, avail = Avail, subs = Subs} = S) ->
    case is_key(Pid, Players) of
	false -> S;
	true -> Pid ! {open_tables, Avail},
	       S#state{subs=sets:add_element(Pid, Subs)}
    end;
cast({unsubscribe, Pid}, #state{subs = Subs} = S) ->
    S#state{subs=sets:del_element(Pid, Subs)}.

% internal functions

start_player_bots(_, 0) ->
    ok;
start_player_bots(TableName, HowMany) when is_integer(HowMany), HowMany > 0 ->
    PlayerName = "player-bot-" ++ integer_to_list(erlang:unique_integer([positive])),
    player:start(PlayerName, TableName),
    start_player_bots(TableName, HowMany - 1).

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
