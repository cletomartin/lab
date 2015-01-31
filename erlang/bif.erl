-module(bif).
-export([integer_div/2]).

integer_div(X, Y) ->
    Int = erlang:trunc(X / Y),
    io:format("Integer part: ~w~n", [Int]).
