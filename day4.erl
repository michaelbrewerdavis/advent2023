-module(day4).
-import(common, [read_file/1]).
-export([main1/1, main2/1]).

parse_input(Filename) ->
    Lines = read_file(Filename),
    lists:map(
        fun(Line) ->
            [Left, Right] = string:split(Line, ":"),
            [_, IdStr] = string:split(Left, " "),
            {Id, _} = string:to_integer(IdStr),
            [Winners, Numbers] = string:split(Right, "|"),
            {Id, split_numbers(Winners), split_numbers(Numbers)}
        end,
        Lines
    ).

split_numbers(S) ->
    Split = lists:filter(
        fun(Str) -> string:is_empty(Str) == false end,
        string:split(string:trim(S), " ", all)
    ),
    Parsed = lists:map(
        fun(Str) ->
            {N, _} = string:to_integer(Str),
            N
        end,
        Split
    ),
    ordsets:from_list(Parsed).

exp(_, 0) ->
    1;
exp(N, M) ->
    N * exp(N, M - 1).

num_matches({_, Winning, Numbers}) ->
    Matches = lists:filter(fun(N) -> ordsets:is_element(N, Winning) end, Numbers),
    length(Matches).

score_card(C) ->
    NumMatches = num_matches(C),
    if
        NumMatches > 0 ->
            exp(2, NumMatches - 1);
        true ->
            0
    end.

main1(Filename) ->
    Cards = parse_input(Filename),
    Scores = lists:map(fun score_card/1, Cards),
    lists:sum(Scores).

apply_card(Cards, _) when length(Cards) == 0 ->
    [];
apply_card(Cards, Counts) ->
    % io:format("Input ~p ~p ~n", [Cards, Counts]),
    [Card | CardsRest] = Cards,
    [Count | CountsRest] = Counts,
    Matches = num_matches(Card),
    AddedCards = lists:duplicate(Matches, Count),
    NewCounts = lists:map(
        fun({L, R}) -> L + R end,
        lists:zip(AddedCards, CountsRest, {pad, {0, 0}})
    ),
    RecursedCount = apply_card(CardsRest, NewCounts),
    [Count | RecursedCount].

main2(Filename) ->
    Cards = parse_input(Filename),
    Base = lists:map(fun(_) -> 1 end, Cards),
    Counts = apply_card(Cards, Base),
    lists:sum(Counts).
