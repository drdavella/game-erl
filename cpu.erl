-module(cpu).
-export([tick/2]).


% Some (very) basic logging machinery
bin_to_int(Bin) when is_binary(Bin) ->
    hd(binary_to_list(Bin));
bin_to_int(Bits) when is_bitstring(Bits) ->
    lists:flatten(io_lib_pretty:print(Bits));
bin_to_int(Int) when is_integer(Int) ->
    Int.

tick(Code, Pc) ->
    {_, Start} = lists:split(Pc, Code),
    [Opcode|Rest] = Start,
    decode(<<Opcode>>, Rest, Pc).

% Disable interrupt
decode(<<16#f3>>, _, Pc) ->
    io:fwrite("disable interrupt~n"),
    Pc + 1;
% ADD operations
decode(<<16#8:4, LowNibble/bits>>, _, Pc) ->
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
