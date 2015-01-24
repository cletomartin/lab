-module(tutorial).
-export([double/1, fac/1, mult/2]).
-export([convert/2, convert_length/1]).
-export([list_length/1]).
-export([list_max/1, list_min/1, list_revert/1]).
-export([format_temp_list/1, convert_to_c/1]).

double(X) ->
    2 * X.

fac(1) ->
    1;
fac(X) ->
    X * fac(X - 1).

mult(X, Y) ->
    X * Y.

convert(In, inch) ->
    In / 2.54;
convert(Cm, cm) ->
    Cm * 2.54.

convert_length({cm, Y}) ->
    {inch, convert(Y, inch)};
convert_length({inch, Y}) ->
    {cm, convert(Y, cm)}.

list_length([]) ->
    0;
list_length([_|T]) ->
    1 + list_length(T).

list_max([H|T]) ->
    list_max(T, H).

list_max([], E) ->
    E;
list_max([H|T], E) when H > E ->
    list_max(T, H);
list_max([_|T], E) ->
    list_max(T, E).

list_min([H|T]) ->
    list_min(T, H).

list_min([], E) ->
    E;
list_min([H|T], E) when H < E ->
    list_min(T, H);
list_min([_|T], E) ->
    list_min(T, E).

list_revert(L) ->
    list_revert(L, []).

list_revert([], E) ->
    E;
list_revert([H|T], E) ->
    list_revert(T, [H | E]).


% This expect a list of temp_data with the follwing format:
%   {city, {type, value}} (type: f|c, value: float)
format_temp_list([]) ->
    ok;
format_temp_list([H|T]) ->
    format_temp(H),
    format_temp_list(T).

format_temp({X, {f, Y}}) ->
    io:format("~-15w ~w F~n", [X, Y]);
format_temp({X, {c, Y}}) ->
    io:format("~-15w ~w C~n", [X, Y]).


convert_to_c([{Name, {f, V}} | Rest]) ->
    New = {Name, {c, (V - 32) * 5 / 9}},
    [New | convert_to_c(Rest)];
convert_to_c([H|T]) ->
    [H | convert_to_c(T)];
convert_to_c([]) ->
    [].

format_temp_ord(List) ->
    C = convert_to_c(List),
    Ord = temp_ord_list(C),
    format_temp_list(Ord).

temp_ord_list(List) ->
    temp_ord_list()
