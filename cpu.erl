-module(cpu).
-export([run/1]).

-define(BYTE_MASK, 16#ff).


% Some (very) basic logging machinery
bin_to_int(Bin) when is_binary(Bin) ->
    hd(binary_to_list(Bin));
bin_to_int(Bits) when is_bitstring(Bits) ->
    lists:flatten(io_lib_pretty:print(Bits));
bin_to_int(Int) when is_integer(Int) ->
    Int.

run(Code) ->
    % For now we're initializing the entire memory array to 0, although that
    % may not end up being a great idea in practice...
    Memory = array:new(1 bsl 16, {default, 0}),
    State = dict:from_list([
        {pc, 0},
        {sp, 0}, % TODO: I believe sp gets initialized by the program itself
        {a, 0},
        {b, 0},
        {c, 0},
        {d, 0},
        {e, 0},
        {h, 0},
        {l, 0},
        {halt, false},
        {mem, Memory}
    ]),
    loop(Code, State).

loop(Code, State) ->
    loop(Code, tick(Code, State)).

tick(Code, State) ->
    case dict:fetch(halt, State) of
        true ->
            io:fwrite("NOP~n"),
            NewState = State;
        false ->
            Pc = dict:fetch(pc, State),
            {_, Start} = lists:split(Pc, Code),
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
get_hl(State) ->
    (dict:fetch(h, State) bsl 8) bor dict:fetch(l, State).
read_mem(State, Addr) ->
    array:get(Addr, dict:fetch(mem, State)).

do_load(State, Dest, Source) when Source == hl ->
    Address = get_hl(State),
    io:fwrite("read memory at addr=~w~n", [Address]),
    dict:store(Dest, read_mem(State, Address), State);
do_load(State, Dest, Source) ->
    io:fwrite("ld ~w->~w~n", [Source, Dest]),
    dict:store(Dest, dict:fetch(Source, State), State).

% Disable interrupt
decode(<<16#f3>>, _, State) ->
    io:fwrite("disable interrupt~n"),
    dict:update_counter(pc, 1, State);
% HALT instruction
decode(<<16#76>>, _, State) ->
    io:fwrite("HALT"),
    % TODO: update this once interrupts actually work
    % NewState = dict:store(halt, true, State),
    % dict:store(pc, dict:fetch(pc, NewState) + 1, NewState);
    dict:update_counter(pc, 1, State);
% ADD operations
decode(<<16#8:4, LowNibble/bits>>, _, State) ->
    io:fwrite("op2=~w~n", [op2(LowNibble)]),
    dict:update_counter(pc, 1, State);
% SUB operations
decode(<<16#9:4, LowNibble/bits>>, _, State) ->
    dict:update_counter(pc, 1, State);
% Bitwise AND operations
decode(<<16#A:4, LowNibble/bits>>, _, State) ->
    dict:update_counter(pc, 1, State);
% Bitwise OR operations
decode(<<16#B:4, LowNibble/bits>>, _, State) ->
    dict:update_counter(pc, 1, State);
% Load operations
decode(<<H:4, LowNibble/bits>>, _, State) when H >= 4, H =< 7 ->
    <<NibbleVal:4/integer>> = LowNibble,
    io:fwrite("LD: 0x~w~.16B: ", [H, NibbleVal]),
    UpdatedState = do_load(State, load_dest(H, LowNibble), op2(LowNibble)),
    dict:update_counter(pc, 1, UpdatedState);
% Unrecognized instruction (error condition, used for development)
decode(Unknown, _, State) ->
    Pc = dict:fetch(pc, State),
    io:format("unknown instruction (pc=~w): 0x~.16B~n",
              [Pc, bin_to_int(Unknown)]),
    dict:update_counter(pc, 1, State).
