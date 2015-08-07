-module(db).

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
  [] = db:match(chelsea, DbRef50),
  ok = db:destroy(DbRef50),
  ok.

new() -> [].

write(Key, Value, DbRef) ->
  [{Key, Value} | DbRef].

read(Key, DbRef) ->
  Filter =
    fun({K, _}) ->
      K =:= Key
    end,
  Elements = lists:filter(Filter, DbRef),
  case Elements of
    [] -> {error, instance};
    [{_, Value}|_] -> {ok, Value}
  end.

delete(Key, DbRef) ->
  Filter =
    fun({K, _}) ->
      K =/= Key
    end,
  lists:filter(Filter, DbRef).

match(_, []) -> [];
match(Value, [{_, DiffValue} | DbRef]) when Value =/= DiffValue ->
  match(Value, DbRef);
match(Value, [{Key, Value} | DbRef]) ->
  CleanDbRef = delete(Key, DbRef),
  [Key | match(Value, CleanDbRef)].

destroy(_) -> ok.
