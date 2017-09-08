-module(cpu).
-export([tick/2]).


tick(Code, Pc) ->
    {_, Start} = lists:split(Pc, Code),
    [Opcode|Rest] = Start,
    decode(<<Opcode>>, Rest, Pc).

decode(<<16#f3>>, _, Pc) ->
    io:fwrite("disable interrupt\n"),
    Pc + 1;
decode(Unknown, _, Pc) ->
    io:format("unknown instruction (pc=~w): 0x~.16B~n",
              [Pc, hd(binary_to_list(Unknown))]),
    Pc + 1.
