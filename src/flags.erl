-module(flags).

-ifdef(TEST).
-compile(export_all).
-else.
-export([zero_flag/2, set_sub_flag/1, half_carry/3, check_condition/2]).
-endif.

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

get_zero_flag(State) ->
    dict:fetch(flags, State) bsr 7.

get_carry_flag(State) ->
    dict:fetch(flags, State) bsr 4.

check_condition(nz, State) ->
    get_zero_flag(State) == 0;
check_condition(nc, State) ->
    get_carry_flag(State) == 1;
check_condition(z, State) ->
    get_zero_flag(State) == 0;
check_condition(c, State) ->
    get_carry_flag(State) == 1.
