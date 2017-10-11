-module(jump).
-import(utils, [update_tick/2]).
-export([jump/3]).


jump(unconditional, Code, State) ->
    Address = get_addr(Code),
    io:fwrite("JP NZ to address = 0x~.16B~n", [Address]),
    NewState = do_jump(true, Address, State),
    update_tick(16, NewState).

do_jump(true, Addr, State) ->
    dict:store(pc, Addr, State);
do_jump(false, _, State) ->
    % We should actually update the tick counter here
    State.

get_addr(Code) ->
    [Low, High | _] = Code,
    (High bsl 8) bor Low.
