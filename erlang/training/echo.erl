% -*- mode:erlang -*-

-module(echo).
-export([go/0, loop/0]).

go() ->
    Pid = spawn(echo, loop, []),
    Pid ! {self(), hello},
    receive
        {Pid, Msq} ->
            io:format("~w~n", [Msq])
    end,
    Pid ! stop.

loop () ->
    receive
        {From, Msq} ->
            From ! {self(), Msq},
            loop();
        stop ->
            true
    end.
