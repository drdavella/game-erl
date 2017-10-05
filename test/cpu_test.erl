-module(cpu_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


decode_nop_test() ->
    State = cpu:init_state(),
    NewState = cpu:decode(<<0>>, [], State),
    % The only thing that happens for a NOP is updating the PC
    NewState = dict:update_counter(pc, 1, State).

-endif.
