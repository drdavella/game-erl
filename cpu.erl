-module(cpu).
-import(jump, [jump/3]).
-import(memory, [load/3, load_imm/3, load_imm_d/3, load_and_update/3]).
-import(utils, [inc16/1, update_tick/2]).
-export([run/1]).

-define(BYTE_MASK, 16#ff).
-define(BOOT_ROM_START, 16#100).


% Some (very) basic logging machinery
bin_to_int(Bin) when is_binary(Bin) ->
    hd(binary_to_list(Bin));
bin_to_int(Bits) when is_bitstring(Bits) ->
    lists:flatten(io_lib_pretty:print(Bits));
bin_to_int(Int) when is_integer(Int) ->
    Int.

% This initializes the processor state to what would be expected after the
% internal boot ROM has executed. We don't emulate that process directly, so
% we make sure we set up the expected state.
init_state() ->
    Memory = array:new(1 bsl 16, {default, 0}),
    dict:from_list([
        {pc, ?BOOT_ROM_START}, % Boot ROM jumps to PC=0x100 when it's done
        {sp, 16#fffe}, % Boot ROM initializes the stack pointer
        % These values all come from the Pan Docs, but programs shouldn't
        % necessarily assume this state will be present.
        {a, 16#01},
        {f, 16#b0},
        {b, 16#00},
        {c, 16#13},
        {d, 16#00},
        {e, 16#d8},
        {h, 16#01},
        {l, 16#4d},
        {halt, false},
        {tick, 0}, % tick counter
        {mem, Memory}
    ]).

run(Code) ->
    % For now we're initializing the entire memory array to 0, although that
    % may not end up being a great idea in practice...
    loop(Code, init_state()).

loop(Code, State) ->
    loop(Code, tick(Code, State)).

tick(Code, State) ->
    case dict:fetch(halt, State) of
        true ->
            io:fwrite("NOP~n"),
            NewState = State;
        false ->
            Pc = dict:fetch(pc, State),
            % For the time being, 0x100 of the cartridge rom is at addr=0x0
            {_, Start} = lists:split(Pc-?BOOT_ROM_START, Code),
            [Opcode|Rest] = Start,
            NewState = decode(<<Opcode>>, Rest, State)
    end,
    NewState.

load_dest(High, LowNibble) ->
    <<Low:4>> = LowNibble,
    lists:nth((High - 4) * 2 + (Low div 8) + 1, [b,c,d,e,h,l,hl,a]).
op2(LowNibble) ->
    <<Index:4>> = LowNibble,
    lists:nth((Index rem 8) + 1, [b,c,d,e,h,l,hl,a]).
increment_pc(State, Inc) ->
    dict:update_counter(pc, Inc, State).

% NOP
decode(<<0>>, _, State) ->
    io:fwrite("NOP~n"),
    increment_pc(State, 1);
% Disable interrupt
decode(<<16#f3>>, _, State) ->
    io:fwrite("disable interrupt~n"),
    increment_pc(State, 1);
% HALT instruction
decode(<<16#76>>, _, State) ->
    io:fwrite("HALT"),
    % TODO: update this once interrupts actually work
    % NewState = dict:store(halt, true, State),
    increment_pc(State, 1);
% JP unconditional
decode(<<16#c3>>, Code, State) ->
    jump(unconditional, Code, State);
% LD (HL+), A
decode(<<16#22>>, _, State) ->
    io:fwrite("LD (HL+), A"),
    NewState = load_and_update(hl, inc, State),
    increment_pc(NewState, 1);
% LD (HL-), A
decode(<<16#32>>, _, State) ->
    io:fwrite("LD (HL-), A"),
    NewState = load_and_update(hl, dec, State),
    increment_pc(NewState, 1);
% INC SP
decode(<<16#33>>, _, State) ->
    io:fwrite("increment stack pointer~n"),
    NewState = dict:update(sp, fun utils:inc16/1, State),
    increment_pc(update_tick(8, NewState), 1);
% ADD operations
decode(<<16#8:4, LowNibble/bits>>, _, State) ->
    io:fwrite("op2=~w~n", [op2(LowNibble)]),
    increment_pc(State, 1);
% SUB operations
decode(<<16#9:4, LowNibble/bits>>, _, State) ->
    increment_pc(State, 1);
% Bitwise AND operations
decode(<<16#A:4, LowNibble/bits>>, _, State) ->
    increment_pc(State, 1);
% Bitwise OR operations
decode(<<16#B:4, LowNibble/bits>>, _, State) ->
    increment_pc(State, 1);
% Load double-word immediates
decode(<<H:4, 16#1:4>>, Code, State) when H =< 3 ->
    io:fwrite("LDD imm: 0x~.16B1: ", [H]),
    NewState = load_imm_d(State, Code, H),
    increment_pc(NewState, 3);
% Load single-word immediates
decode(<<H:4, L:4>>, Code, State)
  when H =< 3, (L == 6 orelse L == 16#e) ->
    io:fwrite("LD imm: 0x~.16B~.16B: ", [H, L]),
    NewState = load_imm(State, Code, [H, L]),
    increment_pc(NewState, 2);
% Loads to/from memory
decode(<<H:4, LowNibble/bits>>, _, State) when H >= 4, H =< 7 ->
    <<NibbleVal:4/integer>> = LowNibble,
    io:fwrite("LD: 0x~.16B~.16B: ", [H, NibbleVal]),
    NewState = load(State, load_dest(H, LowNibble), op2(LowNibble)),
    increment_pc(NewState, 1);
% Unrecognized instruction (error condition, used for development)
decode(Unknown, _, State) ->
    Pc = dict:fetch(pc, State),
    io:format("unknown instruction (pc=0x~.16B): 0x~.16B~n",
              [Pc, bin_to_int(Unknown)]),
    erlang:error(unknown_instruction).
    %increment_pc(State, 1).
