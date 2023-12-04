-module(common).
-export([read_file/1]).

read_file(Filename) ->
    {_, Input} = file:read_file(Filename),
    Lines = lists:filter(fun(X) -> not string:is_empty(X) end, string:split(Input, "\n", all)),
    Lines.
