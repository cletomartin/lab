-module(srv_sup_db).

-behaviour(gen_server).

-export([start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export(
   [test/0, write/3, read/2, delete/2, match/2, destroy/1]
  ).

-export(
   [dbnew/0, dbwrite/3, dbread/2, dbdelete/2, dbmatch/2, dbdestroy/1]
  ).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start(srv_sup_db , noargs, []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init(noargs) ->
    {ok, dbnew()}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({read, Key}, _From, State) ->
    Reply = dbread(Key, State),
    {reply, Reply, State};
handle_call({match, Value}, _From, State) ->
    Reply = dbmatch(Value, State),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({write, Key, Value}, State) ->
    NewState = dbwrite(Key, Value, State),
    {noreply, NewState};
handle_cast({delete, Key}, State) ->
    NewState = dbdelete(Key, State),
    {noreply, NewState};
handle_cast({destroy}, State) ->
    NewState = dbdestroy(State),
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


test() ->
    {ok, SupId} = srv_sup_db:start(),
    {ok, BasketDb} = srv_sup_db:start_db(SupPid, basketball),
    {ok, SoccerDb} = srv_sup_db:start_db(SupPid, soccer),
    ok = srv_db:write(barcelona, basketball, Db),
    {ok, basketball} = srv_db:read(barcelona, Db),
    ok = my_db:delete(barcelona, Db),
    {error, instance} = my_db:read(barcelona, Db),
    ok = my_db:write(river, soccer, Db),
    ok = my_db:write(manchester_city, soccer, Db),
    Result = my_db:match(soccer, Db),
    [] = Result -- [manchester_city, river],
    [] = [manchester_city, river] -- Result,
    [] = my_db:match(chelsew, Db),
    ok = my_db:destroy(Db),
    try my_db:read(something, Db) of
       Something -> throw({unexpected_result, Something})
    catch
       _:_ -> ok
    end,
    ok.

    ok.

write(Key, Value, Db) ->
    gen_server:cast(Db, {write, Key, Value}).

delete(Key, Db) ->
    gen_server:cast(Db, {delete, Key}).

read(Key, Db) ->
    gen_server:call(Db, {read, Key}).

match(Value, Db) ->
    gen_server:call(Db, {match, Value}).

destroy(Db) ->
    gen_server:cast(Db, {destroy}).

















dbnew() ->
    [].

dbwrite(Key, Value, []) ->
    [{Key, Value}];
dbwrite(Key, Value, [{Key, _}|T]) ->
    [{Key, Value}|T];
dbwrite(Key, Value, [H|T]) ->
    [H|write(Key, Value, T)].


dbread(_, []) ->
    {error, instance};
dbread(Key, [{Key, Value}|_]) ->
    {ok, Value};
dbread(Key, [_|T]) ->
    read(Key, T).

dbdelete(_, []) ->
    [];
dbdelete(Key, [{Key, _}|T]) ->
    delete(Key, T);
dbdelete(Key, [Element | T]) ->
    [Element | delete(Key, T)].

dbmatch(_, []) ->
    [];
dbmatch(Value, [{_, Element}|T]) when Value =/= Element ->
    match(Value, T);
dbmatch(Value, [{Key, Value}|T]) ->
    [Key|delete(Key, match(Value, T))].

dbdestroy(_) ->
    ok.
