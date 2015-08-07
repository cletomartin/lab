-module(srv_db).
-behaviour(gen_server).

-export([test/0]).
-export([start/0, start_link/1, write/3, read/2, delete/2, match/2, stop/1]).
-export([ init/1
        , handle_cast/2
        , handle_call/3
        , terminate/2
        , handle_info/2
        , code_change/3
        ]).

test() ->
  {ok, DbRef} = srv_db:start(),
  ok = srv_db:write(barcelona, soccer, DbRef),
  {ok, soccer} = srv_db:read(barcelona, DbRef),
  {error, instance} = srv_db:read(chelsea, DbRef),
  ok = srv_db:write(barcelona, basketball, DbRef),
  {ok, basketball} = srv_db:read(barcelona, DbRef),
  ok = srv_db:delete(barcelona, DbRef),
  {error, instance} = srv_db:read(barcelona, DbRef),
  ok = srv_db:write(river, soccer, DbRef),
  ok = srv_db:write(manchester_city, soccer, DbRef),
  Result = srv_db:match(soccer, DbRef),
  [] = Result -- [manchester_city, river],
  [] = [manchester_city, river] -- Result,
  [] = srv_db:match(chelsea, DbRef),
  ok = srv_db:stop(DbRef),
  try srv_db:read(something, DbRef) of
    Something -> throw({unexpected_result, Something})
  catch
    _:_ -> ok
  end,
  ok.

start() ->
  gen_server:start(srv_db, noargs, []).

start_link(Name) ->
  gen_server:start_link({local, Name}, srv_db, noargs, []).

write(Key, Value, DbPid) ->
  gen_server:cast(
    DbPid, #{action => write, key => Key, value => Value}).

delete(Key, DbPid) ->
  gen_server:cast(DbPid, #{action => delete, key => Key}).

read(Key, DbPid) ->
  gen_server:call(DbPid, #{action => read, key => Key}).

match(Value, DbPid) ->
  gen_server:call(DbPid, #{action => match, value => Value}).

stop(DbPid) -> gen_server:cast(DbPid, #{action => stop}).

init(noargs) ->
  {ok, db:new()}.

handle_cast(Action = #{action := write}, State) ->
  #{key := Key, value := Value} = Action,
  NewState = db:write(Key, Value, State),
  {noreply, NewState};
handle_cast(Action = #{action := delete}, State) ->
  #{key := Key} = Action,
  NewState = db:delete(Key, State),
  {noreply, NewState};
handle_cast(#{action := stop}, State) ->
  {stop, normal, State}.

handle_call(#{action := read, key := Key}, _From, State) ->
  Value = db:read(Key, State),
  {reply, Value, State};
handle_call(#{action := match, value := Value}, _From, State) ->
  Result = db:match(Value, State),
  {reply, Result, State}.

terminate(_Reason, State) ->
  db:destroy(State),
  "ok, it was a nice loop. Bye".

%% Unused callbacks
handle_info(_Msg, State) -> {noreply, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
