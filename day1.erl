-module(day1).
-export([sum/1, sum2/1]).

digits() ->
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"].

replace_strings(S) ->
    D = digits(),
    Regexp = lists:concat([
        "(",
        string:join(digits(), "|"),
        ")"
    ]),
    R = re:replace(
        S,
        Regexp,
        fun(X, _Y) ->
            Digit = string:str(D, [binary_to_list(X)]),
            integer_to_list(Digit)
        end,
        [global, {return, list}]
    ),
    io:format("replaced ~p ~p ~n", [S, R]),
    R.

smush(S) ->
    re:replace(S, "[a-z]", "", [{return, list}, global]).

first_char(S) ->
    string:slice(S, 0, 1).

last_char(S) ->
    string:slice(S, string:length(S) - 1, 1).

number_from_string(S) ->
    Smushed = smush(S),
    F = first_char(Smushed),
    L = last_char(Smushed),
    Joined = string:concat(F, L),
    {N, _} = string:to_integer(Joined),
    N.

sum(Filename) ->
    {_, Input} = file:read_file(Filename),
    Lines = lists:filter(fun(X) -> not string:is_empty(X) end, string:split(Input, "\n", all)),
    Fixed = lists:map(fun replace_strings/1, Lines),
    Nums = lists:map(fun number_from_string/1, Fixed),
    io:format("~p ~n", [lists:zip(Lines, Nums)]),
    Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0, Nums),
    Sum.

all_captures(S, R) ->
    all_captures(S, R, 0).

all_captures(S, R, N) ->
    M = re:run(S, R, [{offset, N}]),
    if
        M == nomatch ->
            [];
        true ->
            {_, [{StartIndex, Len}, _]} = M,
            io:format("~p ~p ~n", [StartIndex, Len]),
            Match = string:slice(S, StartIndex, Len),
            [Match | all_captures(S, R, StartIndex + 1)]
    end.

lookup(S) ->
    D = digits(),
    Index = string:str(D, [binary_to_list(S)]),
    if
        Index == 0 ->
            {I, _} = string:to_integer(S),
            I;
        true ->
            Index
    end.

parse_number(S) ->
    % R = "(1|2|3|4|5|6|7|8|9)",
    R = "(1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine)",
    C = all_captures(S, R),
    io:format("C ~p ~n", [C]),
    Digits = lists:map(fun lookup/1, C),
    io:format("D ~p ~n", [Digits]),
    [First | _] = Digits,
    Last = lists:last(Digits),
    10 * First + Last.

sum2(Filename) ->
    {_, Input} = file:read_file(Filename),
    Lines = lists:filter(fun(X) -> not string:is_empty(X) end, string:split(Input, "\n", all)),
    Numbers = lists:map(fun parse_number/1, Lines),
    lists:sum(Numbers).
