-module(jump).
-export([jump/3]).


jump(unconditional, Code, State) ->
    Address = get_addr(Code),
    io:fwrite("JP NZ to address = 0x~.16B~n", [Address]),
    do_jump(true, Address, State).

do_jump(true, Addr, State) ->
    dict:store(pc, Addr, State);
do_jump(false, _, State) ->
    % We should actually update the tick counter here
    State.

get_addr(Code) ->
    [Low, High | _] = Code,
    Address = (High bsl 8) bor Low.
