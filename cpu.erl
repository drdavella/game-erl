-module(cpu).
-import(memory, [do_load/3]).
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
    % dict:store(pc, dict:fetch(pc, NewState) + 1, NewState);
    increment_pc(State, 1);
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
% Load operations
decode(<<H:4, LowNibble/bits>>, _, State) when H >= 4, H =< 7 ->
    <<NibbleVal:4/integer>> = LowNibble,
    io:fwrite("LD: 0x~w~.16B: ", [H, NibbleVal]),
    UpdatedState = do_load(State, load_dest(H, LowNibble), op2(LowNibble)),
    increment_pc(State, 1);
% Unrecognized instruction (error condition, used for development)
decode(Unknown, _, State) ->
    Pc = dict:fetch(pc, State),
    io:format("unknown instruction (pc=~w): 0x~.16B~n",
              [Pc, bin_to_int(Unknown)]),
    increment_pc(State, 1).
