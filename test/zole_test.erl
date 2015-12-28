-module(zole_test).
-define(CARDS(C),lists:map(fun zole:card/1,C)).
-import(lists,[all/2]).
-import(zole,[sort_cards/1,winner/1,card/1,deck/0,shuffle/1,is_legal_save/2,is_legal_play/3]).
-include_lib("eunit/include/eunit.hrl").

shuffle_test() ->
    Deck = deck(),
    Shuffled = shuffle(Deck),
    ?assert(Deck =/= Shuffled),
    ?assert(length(Deck) == length(Shuffled)),
    ?assert(lists:all(fun(E)-> lists:member(E,Deck) end, Shuffled)).

sort_trump_cards_test() ->
    D = ?CARDS(["J♥","K♦","J♦","10♦","Q♥","7♦","A♦","Q♦","9♦","J♣","8♦","J♠","Q♣","Q♠"]),
    E = ?CARDS(["Q♣","Q♠","Q♥","Q♦","J♣","J♠","J♥","J♦","A♦","10♦","K♦","9♦","8♦","7♦"]),
    ?assertEqual(E, sort_cards(D)).

sort_non_trump_cards_test() ->
    D = ?CARDS(["A♥","10♣","K♥","9♥","A♣","K♣","9♣","9♠","A♠","10♥","K♠"]),
    E = ?CARDS(["A♣","A♠","A♥","10♣","10♥","K♣","K♠","K♥","9♣","9♠","9♥"]),
    ?assertEqual(E, sort_cards(D)).

sort_mixed_cards_test() ->
    D1 = ?CARDS(["A♥","10♣","K♥","9♥","Q♥","7♦","A♦","Q♦","9♦","10♥","K♠"]),
    D2 = ?CARDS(["J♥","A♣","K♣","9♣","9♠","A♠","K♦","J♦","10♦","J♣","8♦","J♠","Q♣","Q♠"]),
    E1 = ?CARDS(["Q♣","Q♠","Q♥","Q♦","J♣","J♠","J♥","J♦","A♦","10♦","K♦","9♦","8♦","7♦"]),
    E2 = ?CARDS(["A♣","A♠","A♥","10♣","10♥","K♣","K♠","K♥","9♣","9♠","9♥"]),
    ?assertEqual(E1 ++ E2, sort_cards(D1 ++ D2)).

first_card_wins_test() ->
    ?assertEqual(card("9♥"), winner(?CARDS(["9♥","10♠","A♣"]))).

second_card_wins_test() ->
    ?assertEqual(card("10♥"), winner(?CARDS(["9♥","10♥","A♣"]))).

trump_wins_test() ->
    ?assertEqual(card("7♦"), winner(?CARDS(["A♥","7♦","10♣"]))).

ace_trump_wins_test() ->
    ?assertEqual(card("A♦"), winner(?CARDS(["9♥","10♦","A♦"]))).

strongest_trump_wins_test() ->
    ?assertEqual(card("Q♦"), winner(?CARDS(["J♦","A♦","Q♦"]))).

clubs_trump_wins_test() ->
    ?assertEqual(card("Q♣"), winner(?CARDS(["Q♠","Q♥","Q♣"]))).

spade_trump_wins_test() ->
    ?assertEqual(card("J♠"), winner(?CARDS(["J♦","J♥","J♠"]))).

is_legal_save_test() ->
    Cards = ?CARDS(["10♥","A♣","A♠","K♥","9♣","9♠","9♥"]),
    ?assert(is_legal_save(?CARDS(["A♣","A♠"]), Cards)),
    ?assert(not is_legal_save(?CARDS(["A♣","A♣"]), Cards)),
    ?assert(not is_legal_save(?CARDS(["10♣","A♠"]), Cards)),
    ?assert(not is_legal_save(?CARDS(["10♣","10♠"]), Cards)).

is_legal_play_test() ->
    Cards = ?CARDS(["10♥","A♣","K♥","Q♣","Q♠","9♥"]),
    ?assert(all(fun(C) -> is_legal_play(C, Cards, []) end, Cards)),
    ?assert(all(fun(C) -> is_legal_play(C, Cards, ?CARDS(["J♠", "A♥"])) end, ?CARDS(["10♥","K♥","9♥"]))),
    ?assert(all(fun(C) -> not is_legal_play(C, Cards, ?CARDS(["J♠", "A♥"])) end, ?CARDS(["A♣","Q♣","Q♠"]))),
    ?assert(all(fun(C) -> is_legal_play(C, Cards, ?CARDS(["A♥", "J♠"])) end, ?CARDS(["Q♣","Q♠"]))),
    ?assert(all(fun(C) -> is_legal_play(C, Cards, ?CARDS(["A♥", "7♦"])) end, ?CARDS(["Q♣","Q♠"]))),
    ?assert(all(fun(C) -> is_legal_play(C, Cards, ?CARDS(["8♦"])) end, ?CARDS(["Q♣","Q♠"]))),
    ?assert(all(fun(C) -> not is_legal_play(C, Cards, ?CARDS(["A♥", "J♠"])) end, ?CARDS(["10♥","A♣","K♥","9♥"]))),
    ?assert(all(fun(C) -> not is_legal_play(C, Cards, ?CARDS(["A♥", "8♦"])) end, ?CARDS(["10♥","A♣","K♥","9♥"]))),
    ?assert(all(fun(C) -> not is_legal_play(C, Cards, ?CARDS(["Q♦"])) end, ?CARDS(["10♥","A♣","K♥","9♥"]))),
    ?assert(all(fun(C) -> is_legal_play(C, Cards, ?CARDS(["A♥", "10♠"])) end, Cards)),
    ?assert(all(fun(C) -> not is_legal_play(C, [], ?CARDS(["A♥", "J♠"])) end, Cards)).
