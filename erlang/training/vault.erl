-module(vault).

-behaviour(gen_server).

-export([
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-export([start/0, in/1, out/0]).

start() ->
  gen_server:start({local, vault}, vault, noargs, []).

in(Value) -> gen_server:cast(vault, {in, Value}).

out() -> gen_server:call(vault, out).


init(noargs) -> {ok, #{value => undefined}}.

handle_call(out, _From, State) ->
  #{value := Value} = State,
  {reply, Value, State}.

handle_cast({in, Value}, State) ->
  NewState = State#{value := Value},
  {noreply, NewState}.

%% Unused callbacks
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
