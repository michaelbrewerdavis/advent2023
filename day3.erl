-module(day3).
-export([main1/1, main2/1]).

-record(entry, {is_symbol, label, row, column, length}).

read_file(Filename) ->
    {_, Input} = file:read_file(Filename),
    Lines = lists:filter(fun(X) -> not string:is_empty(X) end, string:split(Input, "\n", all)),
    Lines.

parse_input(Filename) ->
    Lines = read_file(Filename),
    lists:flatmap(
        fun({Row, Line}) ->
            {Match, Matches} = re:run(Line, "([0-9]+|[^.0-9])", [global]),
            if
                Match == match ->
                    lists:map(
                        fun([{Start, Len} | _]) ->
                            Text = string:slice(Line, Start, Len),
                            {Int, _} = string:to_integer(Text),
                            #entry{
                                is_symbol = Int == error,
                                label =
                                    case Int of
                                        error -> binary_to_list(Text);
                                        _Else -> Int
                                    end,
                                row = Row,
                                column = Start,
                                length = Len
                            }
                        end,
                        Matches
                    );
                true ->
                    []
            end
        end,
        lists:enumerate(0, Lines)
    ).

is_adjacent(Symbol, Model) ->
    #entry{row = SymbolRow, column = SymbolColumn} = Symbol,
    #entry{row = ModelRow, column = ModelColumn, length = ModelLength} = Model,
    (ModelRow > SymbolRow - 2) and
        (ModelRow < SymbolRow + 2) and
        (ModelColumn < SymbolColumn + 2) and
        (ModelColumn + ModelLength > SymbolColumn - 1).

main1(Filename) ->
    Elements = parse_input(Filename),
    {Symbols, Parts} = lists:partition(fun(E) -> E#entry.is_symbol end, Elements),
    Adjacency = lists:map(
        fun(Part) -> {Part, lists:filter(fun(S) -> is_adjacent(S, Part) end, Symbols)} end, Parts
    ),
    Adjacents = lists:filter(fun({_, L}) -> length(L) > 0 end, Adjacency),
    Ids = lists:map(fun({P, _}) -> P#entry.label end, Adjacents),
    lists:sum(Ids).

main2(Filename) ->
    Elements = parse_input(Filename),
    {Symbols, Parts} = lists:partition(fun(E) -> E#entry.is_symbol end, Elements),
    PossibleGears = lists:filter(fun(S) -> S#entry.label == "*" end, Symbols),
    Adjacency = lists:map(
        fun(Gear) -> {Gear, lists:filter(fun(P) -> is_adjacent(Gear, P) end, Parts)} end,
        PossibleGears
    ),
    Gears = lists:filter(fun({_, L}) -> length(L) == 2 end, Adjacency),
    GearRatios = lists:map(
        fun({_, L}) ->
            [P1, P2] = L,
            P1#entry.label * P2#entry.label
        end,
        Gears
    ),
    lists:sum(GearRatios).
