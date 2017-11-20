-module(jump).
-import(utils, [update_tick/2]).
-export([jp/3, jr/5]).

-define(CONDITIONS, [nz, nc, z, c]).


ubyte_to_sbyte(Value) ->
    -(Value band 16#80) + (Value band 16#7f).

do_jump(true, Addr, State) ->
    io:fwrite("to address = 0x~.16B", [Addr]),
    dict:store(pc, Addr, State);
do_jump(false, _, State) ->
    State.

get_addr(Code) ->
    [Low, High | _] = Code,
    (High bsl 8) bor Low.

get_condition(H, L, Offset) ->
    Index = (H - Offset) * 2 + (L div 8),
    lists:nth(Index+1, ?CONDITIONS).

jp(unconditional, Code, State) ->
    update_tick(16, do_jump(true, get_addr(Code), State)).

jr_impl(true, Offset, State) ->
    % The offset is treated as an 8-bit signed integer
    % Account for the size of this instruction in the offset
    NewAddr = ubyte_to_sbyte(Offset) + dict:fetch(pc, State) + 2,
    update_tick(16, do_jump(true, NewAddr, State));
jr_impl(false, _, State) ->
    % No jump, just advance to the next instruction
    NewAddr = dict:fetch(pc, State) + 2, % This instruction is 2 bytes long
    update_tick(12, dict:store(pc, NewAddr, State)).

jr(conditional, H, L, Addr, State) ->
    Condition = get_condition(H, L, 2),
    io:fwrite("~w ", [Condition]),
    jr_impl(flags:check_condition(Condition, State), Addr, State).
