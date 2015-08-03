% -*- mode:erlang -*-

-module(temp).
-export([convert/1, test/0]).

f2c(V) ->
    (5 * (V - 32)) / 9.

c2f(V) ->
    ((9 * V) / 5) + 32.

convert({c, V}) ->
    c2f(V);
convert({f, V}) ->
    f2c(V).

test() ->
    -12.222222222222221 = temp:convert({f, 10}),
    50.0 = temp:convert({c, 10}),
    ok.
