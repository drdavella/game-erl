% main entry point for gamegerl simulator
-module(main).
-export([start/0]).

-define(HELLO, 3).


start() ->
    Code = [16#f00d, 16#face, 16#b00f, 16#bee7],
    io:format("Hello ~w, world!\n", [?HELLO]),
    loop(Code, 0).

process(<<16#f:4, Rest/bits>>) ->
    io:fwrite("It's an 0xf!"),
    Op =
        case Rest of
            <<16#0:4, _/bits>> -> "a";
            <<16#a:4, _/bits>> -> "b"
        end,
    io:fwrite("op = ~w\n", [Op]);
process(<<16#b:4, Rest/bits>>) ->
    io:fwrite("It's a 0xb!");
process(Unknown) ->
    io:format("It's something else: ~w\n", [Unknown]).

loop(Code, 10) ->
    io:fwrite("done!\n");
loop(Code, N) ->
    io:format("~w\n", [N]),
    [Head|Tail] = Code,
    process(<<Head:16>>),
    loop(Tail ++ [Head], N+1).
