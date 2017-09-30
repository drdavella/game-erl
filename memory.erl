-module(memory).
-export([do_load/3]).


get_hl(State) ->
    (dict:fetch(h, State) bsl 8) bor dict:fetch(l, State).

read_mem(State, Addr) ->
    array:get(Addr, dict:fetch(mem, State)).

write_mem(State, Addr, Value) ->
    State.


do_load(State, Dest, Source) when Source == hl ->
    Address = get_hl(State),
    io:fwrite("read memory at addr=~w~n", [Address]),
    dict:store(Dest, read_mem(State, Address), State);
do_load(State, Dest, Source) when Dest == hl ->
    io:fwrite("load memory from ~w!~n", [Source]),
    write_mem(State, get_hl(State), 0);
do_load(State, Dest, Source) ->
    io:fwrite("ld ~w->~w~n", [Source, Dest]),
    dict:store(Dest, dict:fetch(Source, State), State).
