-module(cpu).
-import(memory, [load/3, load_imm/3, load_imm_d/3, load_and_update/3]).
-import(utils, [inc16/1, update_tick/2]).
-import(interrupt, [disable_interrupts/1, process_interrupts/1]).

-include("registers.hrl").

-ifdef(TEST).
-compile(export_all).
-else.
-export([run/1]).
-endif.

%-ifdef(DEBUG).
-define(LOG_OP(OpCode, State),
    io:fwrite("PC=0x~4.16.0b, OP=0x~2.16.0b, flags=0b~4.2b",
              [dict:fetch(pc, State), OpCode,
               dict:fetch(flags, State) bsr 4])).
-define(LOG_DESCRIPTION(Msg), io:fwrite(": ~s", [Msg])).
-define(LOG_NEWLINE, io:fwrite("~n")).
%endif.


-define(BYTE_MASK, 16#ff).
-define(BOOT_ROM_START, 16#100).


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
        {flags, 0},
        {halt, false},
        {tick, 0}, % tick counter
        % registers for control of interrupts
        {di_pending, false},
        {ie_pending, false},
        {ime, 0},
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
            io:fwrite("Halted: NOP~n"),
            NewState = State;
        false ->
            Pc = dict:fetch(pc, State),
            % For the time being, 0x100 of the cartridge rom is at addr=0x0
            {_, Start} = lists:split(Pc-?BOOT_ROM_START, Code),
            [Opcode|Rest] = Start,
            ?LOG_OP(Opcode, State),
            NewState = decode(<<Opcode>>, Rest, State),
            ?LOG_NEWLINE
    end,
    process_interrupts(NewState).

load_dest(High, Low) ->
    lists:nth((High - 4) * 2 + (Low div 8) + 1, ?REGISTERS).
op2(Index) ->
    lists:nth((Index rem 8) + 1, ?REGISTERS).
increment_pc(Inc, State) ->
    dict:update_counter(pc, Inc, State).

% NOP
decode(<<0>>, _, State) ->
    ?LOG_DESCRIPTION("NOP"),
    increment_pc(1, update_tick(4, State));
% Disable interrupt
decode(<<16#f3>>, _, State) ->
    ?LOG_DESCRIPTION("disable interrupt"),
    % This instruction disables interrupts, but not immediately. Interrupts
    % are disabled after instruction after DI is executed.
    increment_pc(1, disable_interrupts(State));
% HALT instruction
decode(<<16#76>>, _, State) ->
    ?LOG_DESCRIPTION("HALT"),
    % TODO: update this once interrupts actually work
    % NewState = dict:store(halt, true, State),
    erlang:error(not_implemented),
    increment_pc(1, State);
% JP unconditional
decode(<<16#c3>>, Code, State) ->
    ?LOG_DESCRIPTION("JP (unconditional) "),
    jump:jp(unconditional, Code, State);
% JR conditional
decode(<<H:4, L:4>>, Code, State)
  when L == 0, H >= 2, H < 4; L == 8, H >= 2, H < 4 ->
    ?LOG_DESCRIPTION("JR (conditional) "),
    jump:jr(conditional, H, L, hd(Code), State);
    % do NOT increment the PC here!!!
% LD (HL+), A
decode(<<16#22>>, _, State) ->
    ?LOG_DESCRIPTION("LD (HL+), A"),
    NewState = load_and_update(hl, inc, State),
    increment_pc(1, NewState);
% LD (HL-), A
decode(<<16#32>>, _, State) ->
    ?LOG_DESCRIPTION("LD (HL-), A"),
    NewState = load_and_update(hl, dec, State),
    increment_pc(1, NewState);
% INC SP
decode(<<16#33>>, _, State) ->
    ?LOG_DESCRIPTION("increment stack pointer"),
    NewState = dict:update(sp, fun utils:inc16/1, State),
    increment_pc(update_tick(8, NewState), 1);
% ADD operations
decode(<<16#8:4, LowNibble:4>>, _, State) ->
    ?LOG_DESCRIPTION(io_lib:format("op2=~w", [op2(LowNibble)])),
    erlang:error(not_implemented),
    increment_pc(1, State);
% SUB operations
decode(<<16#9:4, LowNibble/bits>>, _, State) ->
    ?LOG_DESCRIPTION("SUBTRACT"),
    erlang:error(not_implemented),
    increment_pc(1, State);
% 8-bit DEC register
decode(<<H:4, L:4>>, _, State)
  when H < 16#4, L == 16#5; H < 16#4, L == 16#d ->
    ?LOG_DESCRIPTION("DEC "),
    increment_pc(1, alu:dec(H, L, State));
% Bitwise AND operations
decode(<<16#A:4, LowNibble:4>>, _, State) when LowNibble < 16#8 ->
    ?LOG_DESCRIPTION("BAND"),
    erlang:error(not_implemented),
    increment_pc(1, State);
decode(<<16#A:4, LowNibble:4>>, _, State) when LowNibble >= 16#8 ->
    ?LOG_DESCRIPTION("BXOR"),
    increment_pc(1, alu:xxor(State, op2(LowNibble)));
% Bitwise OR operations
decode(<<16#B:4, LowNibble/bits>>, _, State) ->
    ?LOG_DESCRIPTION("BOR"),
    erlang:error(not_implemented),
    increment_pc(1, State);
% Load double-word immediates
decode(<<H:4, 16#1:4>>, Code, State) when H =< 3 ->
    ?LOG_DESCRIPTION(io_lib:format("LDD imm: 0x~.16B1: ", [H])),
    NewState = load_imm_d(State, Code, H),
    increment_pc(3, NewState);
% Load single-word immediates
decode(<<H:4, L:4>>, Code, State)
  when H =< 3, (L == 6 orelse L == 16#e) ->
    ?LOG_DESCRIPTION(io_lib:format("LD imm: 0x~.16B~.16B: ", [H, L])),
    NewState = load_imm(State, Code, [H, L]),
    increment_pc(2, NewState);
% Loads to/from memory
decode(<<H:4, LowNibble:4>>, _, State) when H >= 4, H =< 7 ->
    ?LOG_DESCRIPTION(io_lib:format("LD: 0x~.16B~.16B: ", [H, LowNibble])),
    NewState = load(State, load_dest(H, LowNibble), op2(LowNibble)),
    increment_pc(1, NewState);
% Unrecognized instruction (error condition, used for development)
decode(Unknown, _, State) ->
    ?LOG_DESCRIPTION("unknown instruction"),
    erlang:error(unknown_instruction).
