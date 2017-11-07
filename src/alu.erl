-module(alu).
-include("registers.hrl").


-ifdef(TEST).
-compile(export_all).
-else.
-export([xxor/2, dec/3]).
-endif.

-define(BYTE_MASK, 16#ff).
-define(ZERO_FLAG, (1 bsl 7)).
-define(SUB_FLAG,  (1 bsl 6)).
-define(HALF_FLAG, (1 bsl 5)).
-define(FULL_FLAG, (1 bsl 4)).


zero_flag(0, State) ->
    io:fwrite(" --set zero flag--"),
    NewFlags = dict:fetch(flags, State) bor ?ZERO_FLAG,
    dict:store(flags, NewFlags, State);
zero_flag(_, State) ->
    NewFlags = dict:fetch(flags, State) band (bnot ?ZERO_FLAG),
    dict:store(flags, NewFlags, State).

set_sub_flag(State) ->
    io:fwrite(" --set sub flag--"),
    NewFlags = dict:fetch(flags, State) bor ?SUB_FLAG,
    dict:store(flags, NewFlags, State).

half_carry(Old, New, State) ->
    Flag = ((Old bxor New) band 16#10) bsr 4,
    OldFlags = dict:fetch(flags, State),
    NewFlags = (OldFlags band (bnot ?HALF_FLAG)) bor (Flag bsl 5),
    dict:store(flags, NewFlags, State).

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
    NewState = half_carry(Old, Result, dict:store(Operand, Result, State)),
    utils:update_tick(4, zero_flag(Result, set_sub_flag(NewState))).

dec(High, Low, State) ->
    utils:update_tick(4, dec_impl(get_incdec_operand(High, Low), State)).

xxor(State, Operand) ->
    Op2 = dict:fetch(Operand, State),
    Result = (Op2 bxor dict:fetch(a, State)) band ?BYTE_MASK,
    NewState = zero_flag(Result, dict:store(a, Result, State)),
    utils:update_tick(4, NewState).
