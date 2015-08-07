-module(temp).

-export([test/0]).
-export([convert/1]).

convert({f, F}) -> {c, (5 * (F - 32)) / 9};
convert({c, C}) -> {f, C * 9 / 5 + 32}.

test() ->
  {c, -12.222222222222221} = temp:convert({f, 10}),
  {f, -2.0} =
    temp:convert({c, -18.88888888888889}),
  ok.
