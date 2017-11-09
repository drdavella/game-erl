-module(alu).
-include("registers.hrl").


-ifdef(TEST).
-compile(export_all).
-else.
-export([xxor/2, dec/3]).
-endif.

-define(BYTE_MASK, 16#ff).


get_incdec_operand(High, Low) ->
    Index = (Low div 8) + High*2,
    lists:nth(Index+1, ?REGISTERS).

dec_impl(hl, State) ->
    erlang:error("not implemented"),
    utils:update_tick(12, State);
dec_impl(Operand, State) ->
    Old = dict:fetch(Operand, State),
    Result = (Old + 16#ff) band ?BYTE_MASK,
    io:fwrite("~w, result=0x~2.16.0B", [Operand, Result]),
    NewState = flags:half_carry(Old, Result, dict:store(Operand, Result, State)),
    utils:update_tick(4, flags:zero_flag(Result, flags:set_sub_flag(NewState))).

dec(High, Low, State) ->
    utils:update_tick(4, dec_impl(get_incdec_operand(High, Low), State)).

xxor(State, Operand) ->
    Op2 = dict:fetch(Operand, State),
    Result = (Op2 bxor dict:fetch(a, State)) band ?BYTE_MASK,
    NewState = flags:zero_flag(Result, dict:store(a, Result, State)),
    utils:update_tick(4, NewState).
