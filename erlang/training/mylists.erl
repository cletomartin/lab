% -*- mode:erlang -*-


% flat([1, [1,2,3], 4]) -> [1,1,2,3,4]

-module(mylists).
-export([printAll/1]).

printAll([]) ->
    io:format("~n", []);
printAll([H|T]) ->
    io:format("~p ", [H]),
    printAll(T).
