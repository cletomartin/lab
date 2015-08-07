-module(demo).

-export([six_if/1]).
-export([times/2]).

six_if(true) -> X = 3, times(X, 2);
six_if(false) -> 5.

times(X, Y) -> Y * X.
