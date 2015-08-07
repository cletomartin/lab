-module(my_db).

-export([test/0, test_helper/2]).
-export([ new/0
        , init/0
        , write/3
        , read/2
        , delete/2
        , match/2
        , destroy/1
        , lock/1
        , unlock/1
        , code_upgrade/1
        ]).

test() ->
  DbRef = my_db:new(),
  ok = my_db:write(barcelona, soccer, DbRef),
  {ok, soccer} = my_db:read(barcelona, DbRef),
  {error, instance} = my_db:read(chelsea, DbRef),
  ok = my_db:write(barcelona, basketball, DbRef),
  {ok, basketball} = my_db:read(barcelona, DbRef),
  ok = my_db:delete(barcelona, DbRef),
  {error, instance} = my_db:read(barcelona, DbRef),
  ok = my_db:write(river, soccer, DbRef),
  ok = my_db:write(manchester_city, soccer, DbRef),
  Result = my_db:match(soccer, DbRef),
  [] = Result -- [manchester_city, river],
  [] = [manchester_city, river] -- Result,
  [] = my_db:match(chelsea, DbRef),
  ok = my_db:lock(DbRef),
  ok = my_db:write(chicago_bulls, basketball, DbRef),
  spawn(my_db, test_helper, [self(), DbRef]),
  receive
    done -> ok
  end,
  {ok, basketball} = my_db:read(chicago_bulls, DbRef),
  my_db:unlock(DbRef),
  timer:sleep(1000),
  {ok, baseball} = my_db:read(chicago_bulls, DbRef),
  ok = my_db:destroy(DbRef),
  try my_db:read(something, DbRef) of
    Something -> throw({unexpected_result, Something})
  catch
    _:_ -> ok
  end,

  NewDbRef = my_db:new(),
  [] = db:new(),
  my_db:write(boston_celtics, basketball, NewDbRef),
  compile:file("db"),
  code:load_abs("db"),
  true = is_integer(db:new()),
  my_db:code_upgrade(NewDbRef),
  {ok, basketball} = my_db:read(boston_celtics, NewDbRef),
  ok.

test_helper(MainTester, DbRef) ->
  ok = my_db:write(chicago_bulls, baseball, DbRef),
  MainTester ! done.


new() -> spawn(my_db, init, []).
write(Key, Value, DbPid) ->
  cast(DbPid, #{action => write, key => Key, value => Value}).
read(Key, DbPid) ->
  call(DbPid, #{action => read, key => Key}).
delete(Key, DbPid) ->
  cast(DbPid, #{action => delete, key => Key}).
match(Value, DbPid) ->
  call(DbPid, #{action => match, value => Value}).
destroy(DbPid) ->
  cast(DbPid, #{action => stop}).
lock(DbPid) ->
  call(DbPid, #{action => lock}).
unlock(DbPid) ->
  cast(DbPid, #{action => unlock}).
code_upgrade(DbPid) ->
  cast(DbPid, #{action => code_upgrade}).

call(DbPid, Message) ->
  DbPid ! Message#{caller => self()},
  receive
    Response -> Response
  after 1000 ->
    throw(timeout)
  end.

cast(DbPid, Message) ->
  DbPid ! Message#{caller => self()},
  ok.

init() ->
  State = db:new(),
  process_flag(trap_exit, true),
  unlocked(State).

unlocked(State) ->
  receive
    #{action := write, key := Key, value := Value} ->
      NewState = db:write(Key, Value, State),
      unlocked(NewState);
    #{action := delete, key := Key} ->
      NewState = db:delete(Key, State),
      unlocked(NewState);
    #{action := read, key := Key, caller := Caller} ->
      Result = db:read(Key, State),
      Caller ! Result,
      unlocked(State);
    #{action := match, value := Value, caller := Caller} ->
      Result = db:match(Value, State),
      Caller ! Result,
      unlocked(State);
    #{action := lock, caller := Caller} ->
      link(Caller),
      Caller ! ok,
      locked(Caller, State);
    #{action := unlock} ->
      unlocked(State);
    {'EXIT', _, _} ->
      unlocked(State);
    #{action := code_upgrade} ->
      NewState = db:code_upgrade(State),
      unlocked(NewState);
    #{action := stop} -> "ok, it was a nice loop. Bye"
  end.

locked(Caller, State) ->
  receive
    {'EXIT', Caller, _} ->
      unlocked(State);
    Msg = #{caller := Caller} ->
      case Msg of
        #{action := write, key := Key, value := Value} ->
          NewState = db:write(Key, Value, State),
          locked(Caller, NewState);
        #{action := delete, key := Key} ->
          NewState = db:delete(Key, State),
          locked(Caller, NewState);
        #{action := read, key := Key, caller := Caller} ->
          Result = db:read(Key, State),
          Caller ! Result,
          locked(Caller, State);
        #{action := match, value := Value, caller := Caller} ->
          Result = db:match(Value, State),
          Caller ! Result,
          locked(Caller, State);
        #{action := lock} ->
          Caller ! ok,
          locked(Caller, State);
        #{action := unlock} ->
          unlink(Caller),
          unlocked(State);
        #{action := code_upgrade} ->
          NewState = db:code_upgrade(State),
          unlocked(NewState);
        #{action := stop} -> "ok, it was a nice loop. Bye"
      end
  end.
