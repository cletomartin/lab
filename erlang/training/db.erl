% -*- mode:erlang -*-

-module(db).

%
% db:new() -> DbRef.
% db:destroy(DbRef) -> ok.
% db:write(Key, Element, DbRef) -> NewDbRef.
% db:delete(Key, DbRef) -> NewDbRef.
% db:read(Key, DbRef) -> {ok, Element} | {error, instance}.
% db:match(Element, DbRef) -> [Key1, ..., KeyN].

-export([test/0, new/0, write/3, read/2, delete/2, match/2, destroy/1]).


test() ->
    DbRef00 = db:new(),
    DbRef10 = db:write(barcelona, soccer, DbRef00),
    {ok, soccer} = db:read(barcelona, DbRef10),
    {error, instance} = db:read(chelsea, DbRef10),
    DbRef20 = db:write(barcelona, basketball, DbRef10),
    {ok, basketball} = db:read(barcelona, DbRef20),
    DbRef30 = db:delete(barcelona, DbRef20),
    {error, instance} = db:read(barcelona, DbRef30),
    DbRef40 = db:write(river, soccer, DbRef30),
    DbRef50 = db:write(manchester_city, soccer, DbRef40),
    Result00 = db:match(soccer, DbRef50),
    [] = Result00 -- [manchester_city, river],
    [] = [manchester_city, river] -- Result00,
    [] = db:match(chelsew, DbRef50),
    ok = db:destroy(DbRef50),
    ok.

new() ->
    [].

write(Key, Value, []) ->
    [{Key, Value}];
write(Key, Value, [{Key, _}|T]) ->
    [{Key, Value}|T];
write(Key, Value, [H|T]) ->
    [H|write(Key, Value, T)].

read(_, []) ->
    {error, instance};
read(Key, [{Key, Value}|_]) ->
    {ok, Value};
read(Key, [_|T]) ->
    read(Key, T).

delete(_, []) ->
    [];
delete(Key, [{Key, _}|T]) ->
    delete(Key, T);
delete(Key, [Element | T]) ->
    [Element | delete(Key, T)].

match(_, []) ->
    [];
match(Value, [{_, Element}|T]) when Value =/= Element ->
    match(Value, T);
match(Value, [{Key, Value}|T]) ->
    [Key|delete(Key, match(Value, T))].

destroy(_) ->
    ok.
