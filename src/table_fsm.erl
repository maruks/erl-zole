-module(table_fsm).
-define(NUMBER_OF_PLAYERS, 3).
-behaviour(gen_fsm).
-import(lists,[delete/2,nth/2,split/2,sort/2,map/2,nth/2,zip/2,member/2,reverse/1,foreach/2]).
-import(maps,[get/2,put/3,from_list/1,to_list/1,is_key/2,keys/1,get/3,update/3]).
-import(zole,[deal_cards/0,deck/0,sort_cards/1,winner/1,is_legal_play/3,is_legal_save/2]).
-export([start_link/1,terminate/3,handle_info/3,code_change/4]).

% FSM states
-export([init/1,wait2join/3,wait2choose/3,wait2save/3,playing/3]).

% API
-export([send_msg/2]).

send_msg(FsmRef, Msg) ->
    gen_fsm:sync_send_event(FsmRef, Msg).

start_link(Name) ->
    gen_fsm:start_link({local, Name}, ?MODULE, {}, []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, wait2join, []}.

wait2join({join}, {From, _}, Players) ->
    io:format("wait2join ~p~n",[From]),
    case member(From, Players) of
	true ->
	    illegal_message(wait2join, Players);
	false ->
	    NextPlayers = [From | Players],
	    if length(NextPlayers) == ?NUMBER_OF_PLAYERS ->
		    new_game(NextPlayers, 1);
	       true ->
		    {reply, {ok}, wait2join, NextPlayers}
	    end
	end;
wait2join({leave}, {From, _}, Players) ->
    io:format("wait2join ~p~n",[From]),
    case member(From, Players) of
	true ->
	    {reply, {ok}, wait2join, delete(From, Players)};
	false ->
	    illegal_message(wait2join, Players)
	end;
wait2join(_, _, S) ->
    illegal_message(wait2join, S).

new_game(Players, First) ->
    {C1, C2, C3, T} = deal_cards(),
    Cards = [C1, C2, C3],
    StateData = {Players, Cards, T, First, 3},
    {reply, {ok}, wait2choose, start_game(StateData)}.

start_game({Players, Cards, Table, Hand, Left}) ->
    CardsMap = from_list(zip(Players,Cards)),
    send_cards(CardsMap),
    send_prompt(nth(Hand, Players), choose),
    {Players, CardsMap, Table, Hand , Left}.

wait2choose(Msg, {Player, _}, {Players, CardsMap, Table, Hand, Left} = S) ->
    case member(Player, Players) andalso nth(Hand, Players) == Player of
	true ->
	    case Msg of
		{zole} ->
		    start_playing(Players, CardsMap, [], {zole, Player}, Hand);
		{lielais} ->
		    NewCards = get(Player, CardsMap) ++ Table,
		    NextCards = put(Player, NewCards, CardsMap),
		    send_cards(Player, NewCards),
		    send_prompt(Player, save),
		    {reply, {ok}, wait2save, {Players, NextCards, [], {lielais, Player}, Hand}};
		 {pass} ->
		    pass(Left - 1, S);
		 _ ->
		    illegal_message(wait2choose, S)
	    end;
       false ->
	    illegal_message(wait2choose, S)
    end.

illegal_message(State, Data) ->
    {reply,{error, illegal_message}, State, Data}.

wait2save({save, Cards}, {Player, _}, {Players, CardsMap, [], {lielais, Player}, Hand} = S) when is_list(Cards) ->
    case member(Player, Players) andalso is_legal_save(Cards, get(Player, CardsMap)) of
	false ->
	    illegal_message(wait2save, S);
	true ->
	    NewCards = get(Player, CardsMap) -- Cards,
	    NextCards = put(Player, NewCards, CardsMap),
	    send_cards(Player, get(Player, NextCards)),
	    start_playing(Players, NextCards, Cards, {lielais, Player}, Hand)
    end;
wait2save(_, _, S) ->
    illegal_message(wait2save, S).

pass(0, {Players, CardsMap, _T, Hand, _L}) ->
    start_playing(Players, CardsMap, [], {galds}, Hand);
pass(NextLeft, {Players, CardsMap, Table, Hand, _}) ->
    NextHand = 1 + (Hand + 1) rem 3,
    send_prompt(nth(NextHand, Players), choose),
    {reply, {pass}, wait2choose, {Players, CardsMap, Table, NextHand, NextLeft}}.

start_playing(Players, CardsMap, Saved, GameType, Hand) ->
    send_prompt(nth(Hand,Players),play),
    send_to_all(Players, {game_type, GameType}),
    {reply, {ok}, playing, {Players, CardsMap, Saved, GameType, Hand, [], maps:new()}}.

table_cards(Table) ->
    map(fun({_Player,Card}) -> Card end,Table).

get_winner(Card,[ {Player, Card} | _R]) ->
    Player;
get_winner(Card,[_P | R]) ->
    get_winner(Card, R).

playing({play, Card}, {From, _}, {Players, CardsMap, _, _, Hand, Table, _} = S) when is_tuple(Card)->
    case nth(Hand,Players) == From andalso is_legal_play(Card, get(From, CardsMap), table_cards(Table)) of
	true ->
	    play(Card, From, S);
	false ->
	    illegal_message(playing, S)
	end;
playing(_ , _,  S) ->
    illegal_message(playing, S).

play(Card, Player, {Players, Cards, Saved, GameType, Hand, Table, Taken}) ->
    NextTable = [{Player, Card} | Table],
    CardsLeft = delete(Card, get(Player, Cards)),
    NextCards = update(Player, CardsLeft, Cards),
    NextHand = 1 + (1 + Hand) rem 3,
    send_cards(Player, CardsLeft),
    send_to_all(Players, {plays, Player, table_cards(NextTable)}),
    case length(NextTable) of
	?NUMBER_OF_PLAYERS ->
	    CardsPlayed = table_cards(NextTable),
	    WinnerCard = winner(reverse(CardsPlayed)),
	    Winner = get_winner(WinnerCard, NextTable),
	    send_to_all(Players, {wins, Winner, CardsPlayed}),
	    NextTaken = put(Winner, [CardsPlayed | get(Winner, Taken, [])], Taken),
	    case length(CardsLeft) of
		0 ->
		    finish_game(Players, Saved, GameType , Taken);
		_ ->
	            send_prompt(nth(NextHand,Players),play),
		    {reply, {ok}, playing, {Players, NextCards, Saved, GameType, NextHand, [], NextTaken}}
	    end;
	_ ->
	    send_prompt(nth(NextHand,Players),play),
	    {reply, {ok}, playing, {Players, NextCards, Saved, GameType, NextHand, NextTable, Taken}}
    end.

finish_game(Players, Saved, GameType, Taken) ->
    send_to_all(Players,{end_of_game, Saved, GameType, Taken}),
    new_game(Players,1).

send_prompt(Player, P) ->
    Player ! {prompt, P}.

send_to_all(Players, Message) ->
    foreach(fun(P) -> P ! Message end, Players).

send_cards(Player, Cards) ->
    Player ! {cards, sort_cards(Cards)}.

send_cards(CardsMap) when is_map(CardsMap) ->
    send_cards(to_list(CardsMap));
send_cards([{P, C} | Tl]) ->
    send_cards(P, C),
    send_cards(Tl);
send_cards([]) ->
    ok.

terminate(shutdown, _StateName, _StateData) ->
    ok.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
