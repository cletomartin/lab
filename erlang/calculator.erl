-module(calculator).
-export([calculator/0]).

calculator() ->
    io:format(
      "Welcome to Erlang Calculator 0.1~n"
      "(Enters END for exit)~n~n", []),
    init_calculator().

init_calculator() ->
    Line = string:strip(io:get_line("> "), both, $\n),
    case Line of
        "END" ->
            io:format("See you!~n", []),
            ok;
        _ ->
            Result = calc(Line), io:format("~s\n", [Result]),
            init_calculator()
    end.

calc(_) -> "0".
