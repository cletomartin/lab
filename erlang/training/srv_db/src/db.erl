-module(db).

-include_lib("stdlib/include/ms_transform.hrl").

-export([ test/0
        , new/0
        , write/3
        , read/2
        , delete/2
        , match/2
        , destroy/1
        , code_upgrade/1
        ]).

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
  [] = db:match(chelsea, DbRef50),
  ok = db:destroy(DbRef50),
  %% Code upgrade
  DbRef60 =
    db:code_upgrade([{boca, soccer}, {liverpool, soccer}]),
  {error, instance} = db:read(barcelona, DbRef60),
  {ok, soccer} = db:read(boca, DbRef60),
  {ok, soccer} = db:read(liverpool, DbRef60),
  ok = db:destroy(DbRef60),
  ok.

new() -> ets:new(db, []).

write(Key, Value, DbRef) ->
  ets:insert(DbRef, {Key, Value}),
  DbRef.

read(Key, DbRef) ->
  case ets:lookup(DbRef, Key) of
    [] -> {error, instance};
    [{Key, Value}] -> {ok, Value}
  end.

delete(Key, DbRef) ->
  ets:delete(DbRef, Key),
  DbRef.

% match(Value, DbRef) ->
%   MatchSpec = {'$1', Value},
%   lists:flatten(ets:match(DbRef, MatchSpec)).

match(Value, DbRef) ->
  MatchSpec =
    ets:fun2ms(fun({K, V}) when V == Value -> K end),
  ets:select(DbRef, MatchSpec).

destroy(DbRef) ->
  ets:delete(DbRef),
  ok.

code_upgrade(Elements) ->
  DbRef = new(),
  Insert =
    fun({Key, Value}) ->
      write(Key, Value, DbRef)
    end,
  lists:foreach(Insert, Elements),
  DbRef.
