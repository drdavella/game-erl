-module(cpu_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


% helper functions for tests
set_reg_value(hl, Value, State) ->
    <<H:8, L:8>> = <<Value:16>>,
    dict:store(h, H, dict:store(l, L, State));
set_reg_value(Reg, Value, State) ->
    dict:store(Reg, Value, State).
update_pc_and_tick(NewPc, NewTick, State) ->
    dict:update_counter(pc, NewPc, dict:update_counter(tick, NewTick, State)).
set_mem_addr(Address, NewValue, State) ->
    NewMem = array:set(Address, NewValue, dict:fetch(mem, State)),
    dict:store(mem, NewMem, State).

nop_test() ->
    State = cpu:init_state(),
    NewState = cpu:decode(<<0>>, [], State),
    % The only thing that happens for a NOP is updating the PC and tick
    NewState = dict:update_counter(pc, 1, utils:update_tick(4, State)).

load_and_update_test() ->
    Address = 16#f00d,
    NewValue = 16#beef,
    % Set up the starting state
    S0 = set_reg_value(hl, Address, cpu:init_state()),
    StartState = set_reg_value(a, NewValue, S0),

    % Set up the test state and run the test
    NewState = cpu:decode(<<16#22>>, [], StartState),
    % Make sure PC has been updated properly
    16#101 = dict:fetch(pc, NewState),
    % Make sure tick has been updated properly
    8 = dict:fetch(tick, NewState),
    % Make sure HL has been updated properly
    ExpAddr = Address + 1,
    ExpAddr = (dict:fetch(h, NewState) bsl 8) bor dict:fetch(l, NewState),
    % Make sure memory has been updated properly
    NewValue = array:get(Address, dict:fetch(mem, NewState)).

-endif.
