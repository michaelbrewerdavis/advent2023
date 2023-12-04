-module(day2).
-export([main1/1, main2/1]).

read_file(Filename) ->
    {_, Input} = file:read_file(Filename),
    Lines = lists:filter(fun(X) -> not string:is_empty(X) end, string:split(Input, "\n", all)),
    Lines.

parse_result(R) ->
    Colors = string:split(R, ",", all),
    ColorMap = lists:foldl(
        fun(C, Acc) ->
            {N, Rest} = string:to_integer(string:trim(C)),
            Color = string:trim(binary_to_list(Rest)),
            Acc#{Color => N}
        end,
        #{"red" => 0, "green" => 0, "blue" => 0},
        Colors
    ),
    ColorMap.

parse_line(Line) ->
    [GameStr, ResultsStr] = string:split(Line, ":"),
    [_, IdStr] = string:split(GameStr, " "),
    {Id, _} = string:to_integer(IdStr),
    ResultsStrs = string:split(ResultsStr, ";", all),
    Results = lists:map(fun parse_result/1, ResultsStrs),
    {Id, Results}.

parse_input(Filename) ->
    Lines = read_file(Filename),
    lists:map(fun parse_line/1, Lines).

result_valid_for_reality(Result, Reality) ->
    lists:all(
        fun(Key) ->
            NumInResult = maps:get(Key, Result),
            NumInReality = maps:get(Key, Reality, 0),
            NumInResult =< NumInReality
        end,
        maps:keys(Result)
    ).

game_valid_for_reality(Results, Reality) ->
    lists:all(fun(R) -> result_valid_for_reality(R, Reality) end, Results).

main1(Filename) ->
    Games = parse_input(Filename),
    Reality = #{"green" => 13, "blue" => 14, "red" => 12},
    Valid = lists:filter(fun({_, Results}) -> game_valid_for_reality(Results, Reality) end, Games),
    ValidIds = lists:map(fun({Id, _}) -> Id end, Valid),
    lists:sum(ValidIds).

minimal_set(Results) ->
    Base = #{"red" => 0, "green" => 0, "blue" => 0},
    lists:foldl(
        fun(Result, Acc) ->
            #{"red" := ResultRed, "green" := ResultGreen, "blue" := ResultBlue} = Result,
            #{"red" := AccRed, "green" := AccGreen, "blue" := AccBlue} = Acc,
            #{
                "red" => max(ResultRed, AccRed),
                "green" => max(ResultGreen, AccGreen),
                "blue" => max(ResultBlue, AccBlue)
            }
        end,
        Base,
        Results
    ).

power(Results) ->
    #{"red" := Red, "blue" := Blue, "green" := Green} = minimal_set(Results),
    Red * Blue * Green.

main2(Filename) ->
    Games = parse_input(Filename),
    Powers = lists:map(fun({_, Results}) -> power(Results) end, Games),
    lists:sum(Powers).
