-module(cpu).
-export([tick/1]).

tick(<<16#f:4, Rest/bits>>) ->
    io:fwrite("It's an 0xf!"),
    Op =
        case Rest of
            <<16#0:4, _/bits>> -> "a";
            <<16#a:4, _/bits>> -> "b"
        end,
    io:fwrite("op = ~w\n", [Op]);
tick(<<16#b:4, Rest/bits>>) ->
    io:fwrite("It's a 0xb!");
tick(Unknown) ->
    io:format("It's something else: ~w\n", [Unknown]),
    io:fwrite("hey there now\n").
