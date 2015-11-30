-module(zole).
-import(lists,[delete/2, nth/2]).
-import(rand,[uniform/1]).

-export([deck/0,shuffle/1]).

is_trump({R, S}) ->
    S==queen orelse S==jack orelse R==$♦.

deck() ->
    [ {7, $♦}, {8, $♦} | [ {R, S} || S <- "♥♦♣♠", R <- [ace, king, queen, jack, 10, 9]]].

shuffle([]) ->
    [];
shuffle(Xs) ->
    E = nth(uniform(length(Xs)), Xs),
    [ E | shuffle(delete(E, Xs))].
