% -*- mode:erlang -*-

-module(db3).

-include_lib("stdlib/include/ms_transform.hrl").

% with ets
% db:new() -> DbRef.
% db:destroy(DbRef) -> ok.
% db:write(Key, Element, DbRef) -> NewDbRef.
% db:delete(Key, DbRef) -> NewDbRef.
% db:read(Key, DbRef) -> {ok, Element} | {error, instance}.
% db:match(Element, DbRef) -> [Key1, ..., KeyN].

-export([test/0, new/0, write/3, read/2, delete/2, match/2, destroy/1]).

test() ->
    DbRef00 = db3:new(),
    DbRef10 = db3:write(barcelona, soccer, DbRef00),
    {ok, soccer} = db3:read(barcelona, DbRef10),
    {error, instance} = db3:read(chelsea, DbRef10),
    DbRef20 = db3:write(barcelona, basketball, DbRef10),
    {ok, basketball} = db3:read(barcelona, DbRef20),
    DbRef30 = db3:delete(barcelona, DbRef20),
    {error, instance} = db3:read(barcelona, DbRef30),
    DbRef40 = db3:write(river, soccer, DbRef30),
    DbRef50 = db3:write(manchester_city, soccer, DbRef40),
    Result00 = db3:match(soccer, DbRef50),
    [] = Result00 -- [manchester_city, river],
    [] = [manchester_city, river] -- Result00,
    [] = db3:match(chelsew, DbRef50),
    ok = db3:destroy(DbRef50),
    ok.

new() ->
    ets:new(db, []).

write(Key, Value, Db) ->
    ets:insert(Db, {Key, Value}),
    Db.

read(Key, Db) ->
    case ets:lookup(Db, Key) of
        [] -> {error, instance};
        [{Key, V}] -> {ok, V}
    end.

delete(Key, Db) ->
    ets:delete(Db, Key),
    Db.

match(Value, Db) ->
%    lists:flatten(ets:match(Db, {'$1', Value})).
    MS = ets:fun2ms(fun({K, V}) when V =:= Value -> K end),
    ets:select(Db, MS).

destroy(_) ->
    ok.
