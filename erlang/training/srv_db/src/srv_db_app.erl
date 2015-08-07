-module(srv_db_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_, _) -> srv_db_sup:start().

stop(_) -> ok.
