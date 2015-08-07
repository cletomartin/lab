-module(srv_db_sup).

-export([test/0]).
-export([start/0, start_db/2]).
-export([init/1]).

test() ->
  Self = self(),
  spawn(fun() -> Self ! do_test() end),
  receive
    Result -> Result
  end.

do_test() ->
  {ok, SupPid} = srv_db_sup:start(),
  {ok, _} = srv_db_sup:start_db(SupPid, basketball),
  {ok, SoccerDb} = srv_db_sup:start_db(SupPid, soccer),
  ok = srv_db:write(chicago_bulls, usa, basketball),
  ok = srv_db:write(real_madrid, spain, basketball),
  {ok, usa} = srv_db:read(chicago_bulls, basketball),
  {error, instance} = srv_db:read(chicago_bulls, soccer),
  exit(SoccerDb, kill),
  timer:sleep(1000),
  ok = srv_db:write(tigres, mexico, soccer),
  {ok, mexico} = srv_db:read(tigres, soccer),
  ok = srv_db:stop(basketball),
  try srv_db:read(chicago_bulls, basketball) of
    Something -> throw({unexpected_result, Something})
  catch
    _:_ -> ok
  end,
  ok.

start() ->
  supervisor:start_link({local, srv_db_sup}, srv_db_sup, noargs).

start_db(SupPid, DbName) ->
  supervisor:start_child(SupPid, [DbName]).

init(noargs) ->
  RestartStrategy = {simple_one_for_one, 10, 3600},
  SrvDbSpec =
    { srv_db
    , {srv_db, start_link, []}
    , transient
    , brutal_kill
    , worker
    , [srv_db, db]
    },
  SupervisorSpec = {RestartStrategy, [SrvDbSpec]},
  {ok, SupervisorSpec}.




