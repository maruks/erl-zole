-module(zole).
-import(lists,[delete/2, nth/2, split/2, sort/2]).
-import(rand,[uniform/1]).
%-export([deck/0,shuffle/1]).
-compile(export_all).
-define(HEARTS, $♥).
-define(DIAMONDS, $♦).
-define(SPADES, $♠).
-define(CLUBS, $♣).

points(ace) -> 11;
points(king) -> 4;
points(queen) -> 3;
points(jack) -> 2;
points(10) -> 10;
points(_) -> 0.

split_all([N | Ns], Xs) ->
    {First , Rest} = split(N, Xs),
    [First | split_all(Ns, Rest)];
split_all(_, Xs) ->
    [Xs].

strength(?CLUBS) ->
    4;
strength(?SPADES) ->
    3;
strength(?HEARTS) ->
    2;
strength(?DIAMONDS) ->
    1;
strength(queen) ->
    130;
strength(jack) ->
    120;
strength(ace) ->
    110;
strength(king) ->
    95;
strength(N) ->
    N*10.

ord({R1, S1}=C1, {R2, S2}=C2) ->
    trump(C1) + strength(R1) + strength(S1) >= trump(C2) + strength(R2) + strength(S2).

trump({R, S}) when R==queen ; R==jack ; S==?DIAMONDS ->
    1000;
trump(_) ->
    0.

sort_cards(Cards) ->
    sort(fun ord/2, Cards).

deck() ->
    [ {7, ?DIAMONDS}, {8, ?DIAMONDS} | [ {R, S} || S <- "♥♦♣♠", R <- [ace, king, queen, jack, 10, 9]]].

shuffle([]) ->
    [];
shuffle(Xs) ->
    E = nth(uniform(length(Xs)), Xs),
    [ E | shuffle(delete(E, Xs))].

deal_cards() ->
    list_to_tuple(split_all([8,8,8], shuffle(deck()))).
