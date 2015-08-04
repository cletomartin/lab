% -*- mode:erlang -*-

-module(db2).

%
% db2:new() -> DbRef.
% db2:destroy(DbRef) -> ok.
% db2:write(Key, Element, DbRef) -> NewDbRef.
% db2:delete(Key, DbRef) -> NewDbRef.
% db2:read(Key, DbRef) -> {ok, Element} | {error, instance}.
% db2:match(Element, DbRef) -> [Key1, ..., KeyN].

-export([test/0, new/0, write/3, read/2, delete/2, match/2, destroy/1]).


test() ->
    DbRef00 = db2:new(),
    DbRef10 = db2:write(barcelona, soccer, DbRef00),
    {ok, soccer} = db2:read(barcelona, DbRef10),
    {error, instance} = db2:read(chelsea, DbRef10),
    DbRef20 = db2:write(barcelona, basketball, DbRef10),
    {ok, basketball} = db2:read(barcelona, DbRef20),
    DbRef30 = db2:delete(barcelona, DbRef20),
    {error, instance} = db2:read(barcelona, DbRef30),
    DbRef40 = db2:write(river, soccer, DbRef30),
    DbRef50 = db2:write(manchester_city, soccer, DbRef40),
    Result00 = db2:match(soccer, DbRef50),
    [] = Result00 -- [manchester_city, river],
    [] = [manchester_city, river] -- Result00,
    [] = db2:match(chelsew, DbRef50),
    ok = db2:destroy(DbRef50),
    ok.

new() ->
    [].

read(Key, DbRef) ->
    Elements = lists:filter(fun({K, _}) -> K =:= Key end, DbRef),
    case Elements of
        [] ->{error, instance};
        [{_, Value}|_] -> {ok, Value}
    end.

delete(Key, DbRef) ->
    lists:filter(fun({K, _}) -> K =/= Key end, DbRef).

write(Key, Value, []) ->
    [{Key, Value}];
write(Key, Value, [{Key, _}|T]) ->
    [{Key, Value}|T];
write(Key, Value, [H|T]) ->
    [H|write(Key, Value, T)].

match(_, []) ->
    [];
match(Value, [{_, Element}|T]) when Value =/= Element ->
    match(Value, T);
match(Value, [{Key, Value}|T]) ->
    [Key|delete(Key, match(Value, T))].

destroy(_) ->
    ok.
