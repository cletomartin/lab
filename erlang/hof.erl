-module(hof).
-export([twice/1]).


twice(List) ->
    Twice = fun(N) -> N * 2 end,
    lists:map(Twice, List).
