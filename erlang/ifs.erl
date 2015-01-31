-module(ifs).
-export([echo/0, is_pair/0, is_pair_case/0]).

echo() ->
    Phrase = string:strip(io:get_line("Say something: "), both, $\n),
    io:format("You said: ~s~n", [Phrase]).

is_pair() ->
    {Number, _} = string:to_integer(
                    string:strip(io:get_line("Say a number: "), both, $\n)),
    if
        Number rem 2 == 0 ->
            io:format("The number ~w is pair~n", [Number]);
        Number == error ->
            io:format("Not a number. Try it again...~n"),
            is_pair();
        true ->
            io:format("The number ~w is not pair~n", [Number])
    end.

is_pair_case() ->
    {Number, _} = string:to_integer(
                    string:strip(io:get_line("Say a number: "), both, $\n)),
    case Number rem 2 of
        0 ->
            io:format("The number ~w is pair~n", [Number]);
        1 ->
            io:format("The number ~w is not pair~n", [Number]);
    end.
