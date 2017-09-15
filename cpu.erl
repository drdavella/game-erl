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
    State = dict:from_list([{pc, 0}]),
    loop(Code, State).

loop(Code, State) ->
    loop(Code, tick(Code, State)).

tick(Code, State) ->
    Pc = dict:fetch(pc, State),
    {_, Start} = lists:split(Pc, Code),
    [Opcode|Rest] = Start,
    dict:store(pc, decode(<<Opcode>>, Rest, Pc), State).

op2(LowNibble) ->
    <<Index:4>> = LowNibble,
    lists:nth((Index rem 8) + 1, [b,c,d,e,h,l,hl,a]).

% Disable interrupt
decode(<<16#f3>>, _, Pc) ->
    io:fwrite("disable interrupt~n"),
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
decode(<<16#5:4, LowNibble/bits>>, _, Pc) ->
    <<NibbleVal:4/integer>> = LowNibble,
    io:fwrite("LD: 0x5~.16B~n", [NibbleVal]),
    Pc + 1;
% Unrecognized instruction (error condition, used for development)
decode(Unknown, _, Pc) ->
    io:format("unknown instruction (pc=~w): 0x~.16B~n",
              [Pc, bin_to_int(Unknown)]),
    Pc + 1.
