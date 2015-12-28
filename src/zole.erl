-module(zole).
-import(lists,[delete/2,nth/2,split/2,sort/2,map/2,filter/2]).
-import(rand,[uniform/1]).
-export([deck/0,deal_cards/0,sort_cards/1,points/1,winner/1,card/1,shuffle/1,is_legal_play/3,is_legal_save/2]).

% API

card([$♦]) -> diamonds;
card([$♥]) -> hearts;
card([$♣]) -> clubs;
card([$♠]) -> spades;
card([$A | S]) ->
    {ace, card(S)};
card([$K | S]) ->
    {king, card(S)};
card([$Q | S]) ->
    {queen, card(S)};
card([$J | S]) ->
    {jack, card(S)};
card([$1, $0 | S]) ->
    {10, card(S)};
card([N | S]) ->
    {list_to_integer([N]), card(S)}.

points(ace) -> 11;
points(king) -> 4;
points(queen) -> 3;
points(jack) -> 2;
points(10) -> 10;
points(_) -> 0.

is_legal_save([C1, C2] = Cards, PlayersCards) ->
    C1 =/= C2 andalso lists:all(fun(E) -> lists:member(E, PlayersCards) end, Cards);
is_legal_save(_, _) ->
    false.

is_legal_play(Card, PlayersCards, Table) ->
    HasCard = lists:member(Card, PlayersCards),
    HasCard andalso is_required_card(Card, Table, PlayersCards).

sort_cards(Cards) ->
    sort(fun ord/2, Cards).

deck() ->
    [ {7, diamonds}, {8, diamonds} | [ {R, S} || S <- [hearts, diamonds, spades, clubs], R <- [ace, king, queen, jack, 10, 9]]].

shuffle([]) ->
    [];
shuffle(Xs) ->
    E = nth(uniform(length(Xs)), Xs),
    [ E | shuffle(delete(E, Xs))].

deal_cards() ->
    list_to_tuple(split_all([8,8,8], shuffle(deck()))).

winner([FirstCard | _Rest] = Cards) ->
    T = map(fun(C) -> {C, strength(FirstCard, C)} end, Cards),
    element(1, hd(sort(fun({_C1, S1}, {_C2, S2}) -> S1 > S2 end, T))).

% private functions

is_required_card(_ , [], _) ->
    true;
is_required_card({_, Cs} = Card, Table, PlayersCards) ->
    FirstCard = {_, Fs} = lists:last(Table),
    case is_trump(FirstCard) of
	true ->
	    req_check(lists:any(fun is_trump/1, PlayersCards), is_trump(Card));
	false ->
	    NonTrumpCards = filter(fun(C) -> not is_trump(C) end, PlayersCards),
	    req_check(lists:any(fun({_,S}) -> S == Fs end, NonTrumpCards), Cs == Fs )
    end.

req_check(true, false) -> false;
req_check(_, _) -> true.

split_all([N | Ns], Xs) ->
    {First , Rest} = split(N, Xs),
    [First | split_all(Ns, Rest)];
split_all(_, Xs) ->
    [Xs].

strength({R, S} = C) ->
    trump(C) + strength(R) + strength(S);
strength(clubs) -> 4;
strength(spades) -> 3;
strength(hearts) -> 2;
strength(diamonds) -> 1;
strength(queen) -> 130;
strength(jack) -> 120;
strength(ace) -> 110;
strength(king) -> 95;
strength(N) -> N * 10.

strength(_FirstPlayedCard ,{R, S} = C) when R==queen; R==jack; S==diamonds ->
    strength(C);
strength({_R1, S}, {_R2, S} = C) ->
    strength(C);
strength(_, _) ->
    0.

ord(Card1, Card2) ->
    strength(Card1) >= strength(Card2).

is_trump({R, S}) ->
    R==queen orelse R==jack orelse S==diamonds.

trump(C) ->
    case is_trump(C) of
	true -> 1000;
	false -> 0
    end.
