-module(cpu).
-export([run/1]).


% Some (very) basic logging machinery
bin_to_int(Bin) when is_binary(Bin) ->
    hd(binary_to_list(Bin));
bin_to_int(Bits) when is_bitstring(Bits) ->
    lists:flatten(io_lib_pretty:print(Bits));
bin_to_int(Int) when is_integer(Int) ->
    Int.

run(Code) ->
    State = dict:from_list([
        {pc, 0}, {a, 0}, {b, 0}, {c, 0}, {d, 0}, {e, 0}, {h, 0}, {l, 0}]),
    loop(Code, State).

loop(Code, State) ->
    loop(Code, tick(Code, State)).

tick(Code, State) ->
    Pc = dict:fetch(pc, State),
    {_, Start} = lists:split(Pc, Code),
    [Opcode|Rest] = Start,
    dict:store(pc, decode(<<Opcode>>, Rest, Pc), State).

load_dest(High, LowNibble) ->
    <<Low:4>> = LowNibble,
    lists:nth((High - 4) * 2 + (Low div 8) + 1, [b,c,d,e,h,l,hl,a]).
op2(LowNibble) ->
    <<Index:4>> = LowNibble,
    lists:nth((Index rem 8) + 1, [b,c,d,e,h,l,hl,a]).

do_load(Dest, Source) when Source == hl ->
    io:fwrite("read memory!~n");
do_load(Dest, Source) ->
    io:fwrite("ld ~w->~w~n", [Source, Dest]).

% Disable interrupt
decode(<<16#f3>>, _, Pc) ->
    io:fwrite("disable interrupt~n"),
    Pc + 1;
% HALT instruction
decode(<<16#76>>, _, Pc) ->
    io:fwrite("HALT"),
    % TODO: This is not the correct behavior, but is just a workaround for now
    Pc + 1;
% ADD operations
decode(<<16#8:4, LowNibble/bits>>, _, Pc) ->
    io:fwrite("op2=~w~n", [op2(LowNibble)]),
    Pc + 1;
% SUB operations
decode(<<16#9:4, LowNibble/bits>>, _, Pc) ->
    Pc + 1;
% Bitwise AND operations
decode(<<16#A:4, LowNibble/bits>>, _, Pc) ->
    Pc + 1;
% Bitwise OR operations
decode(<<16#B:4, LowNibble/bits>>, _, Pc) ->
    Pc + 1;
% Load operations
decode(<<H:4, LowNibble/bits>>, _, Pc) when H >= 4, H =< 7 ->
    <<NibbleVal:4/integer>> = LowNibble,
    io:fwrite("LD: 0x~w~.16B: ", [H, NibbleVal]),
    do_load(load_dest(H, LowNibble), op2(LowNibble)),
    Pc + 1;
% Unrecognized instruction (error condition, used for development)
decode(Unknown, _, Pc) ->
    io:format("unknown instruction (pc=~w): 0x~.16B~n",
              [Pc, bin_to_int(Unknown)]),
    Pc + 1.
