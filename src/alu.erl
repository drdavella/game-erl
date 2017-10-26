-module(alu).

-ifdef(TEST).
-compile(export_all).
-else.
-export([xxor/2]).
-endif.

-define(BYTE_MASK, 16#ff).
-define(ZERO_FLAG, 16#80).


zero_flag(0, State) ->
    io:fwrite(" --set zero flag--"),
    NewFlags = dict:fetch(flags, State) bor ?ZERO_FLAG,
    dict:store(flags, NewFlags, State);
zero_flag(_, State) ->
    NewFlags = dict:fetch(flags, State) band (bnot ?ZERO_FLAG),
    dict:store(flags, NewFlags, State).


xxor(State, Operand) ->
    Op2 = dict:fetch(Operand, State),
    Result = (Op2 bxor dict:fetch(a, State)) band ?BYTE_MASK,
    NewState = zero_flag(Result, dict:store(a, Result, State)),
    utils:update_tick(4, NewState).
