% -*- mode:erlang -*-

-module(basics).

A = 1.
B = 2.
A + B.
% A = A + 1.  This will fail as A is inmutable

L = [A|[2,3]].
[[3,2]|1].
[H|T] = L.
