% -*- mode:erlang -*-

-module(my_db).

-export([test/0, init/0, new/0, write/3, read/2, delete/2, match/2, destroy/1]).


new() ->
    spawn(my_db, init, []).

init() ->
    State = db:new(),
    loop(State).

send_and_wait(Db, Args) ->
    Db ! [self() | Args],
    receive
        R -> R
    after 1000 ->
            throw({error, timeout})
    end.

write(Key, Value, Db) ->
    Db ! {write, Key, Value, self()},
    send_and_wait(Db, [write, Key, Value]).

read(Key, Db) ->
    send_and_wait(Db, [read, Key]).

delete(Key, Db) ->
    send_and_wait(Db, [delete, Key]).

match(Element, Db) ->
    send_and_wait(Db, [match, Element]).

destroy(Db) ->
    send_and_wait(Db, [destroy]).


loop(State) ->
    receive
        [Pid, write, Key, Value] ->
            NewState = db:write(Key, Value, State),
            Pid ! ok,
            loop(NewState);
        [Pid, delete, Key] ->
            NewState = db:delete(Key, State),
            Pid ! ok,
            loop(NewState);
        [Pid, read, Key] ->
            Result = db:read(Key, State),
            Pid ! Result,
            loop(State);
        [Pid, match, Element] ->
            Result = db:match(Element, State),
            Pid ! Result,
            loop(State);
        [Pid, destroy] -> Result = db:destroy(State),
                          Pid ! Result
    end.



test() ->
    Db = my_db:new(),
    ok = my_db:write(barcelona, soccer, Db),
    {ok, soccer} = my_db:read(barcelona, Db),
    {error, instance} = my_db:read(chelsea, Db),
    ok = my_db:write(barcelona, basketball, Db),
    {ok, basketball} = my_db:read(barcelona, Db),
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
