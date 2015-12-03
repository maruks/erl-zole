-module(zole_test).
-include_lib("eunit/include/eunit.hrl").

sort_trump_cards_test() ->
    D = [{jack, $♥}, {king, $♦}, {jack, $♦}, {10, $♦}, {queen, $♥}, {7, $♦}, {ace, $♦}, {queen, $♦}, {9, $♦}, {jack, $♣}, {8, $♦}, {jack, $♠}, {queen, $♣}, {queen, $♠}],
    E = [{queen, $♣}, {queen, $♠}, {queen, $♥}, {queen, $♦}, {jack, $♣}, {jack, $♠}, {jack, $♥}, {jack, $♦}, {ace, $♦}, {10, $♦}, {king, $♦}, {9, $♦}, {8, $♦}, {7, $♦}],
    ?assertEqual(E, zole:sort_cards(D)).

sort_non_trump_cards_test() ->
    D = [{ace, $♥}, {10, $♣}, {king, $♥}, {9, $♥},   {ace, $♣}, {king, $♣}, {9, $♣},     {9, $♠}, {ace, $♠}, {10, $♥} ,{king, $♠}],
    E = [{ace, $♣}, {ace, $♠},{ace, $♥},  {10, $♣}, {10, $♥}, {king, $♣}, {king, $♠}, {king, $♥}, {9, $♣}, {9, $♠},   {9, $♥}],
    ?assertEqual(E, zole:sort_cards(D)).

sort_mixed_cards_test() ->
    D1 = [{ace, $♥}, {10, $♣}, {king, $♥}, {9, $♥}, {queen, $♥}, {7, $♦}, {ace, $♦}, {queen, $♦}, {9, $♦} , {10, $♥} ,{king, $♠}],
    D2 = [{jack, $♥}, {ace, $♣}, {king, $♣}, {9, $♣}, {9, $♠}, {ace, $♠}, {king, $♦}, {jack, $♦}, {10, $♦}, {jack, $♣}, {8, $♦}, {jack, $♠}, {queen, $♣}, {queen, $♠}],
    E1 = [{queen, $♣}, {queen, $♠}, {queen, $♥}, {queen, $♦}, {jack, $♣}, {jack, $♠}, {jack, $♥}, {jack, $♦}, {ace, $♦}, {10, $♦}, {king, $♦}, {9, $♦}, {8, $♦}, {7, $♦}],
    E2 = [{ace, $♣}, {ace, $♠},{ace, $♥},  {10, $♣}, {10, $♥}, {king, $♣}, {king, $♠}, {king, $♥}, {9, $♣}, {9, $♠}, {9, $♥}],
    ?assertEqual(E1 ++ E2, zole:sort_cards(D1 ++ D2)).
