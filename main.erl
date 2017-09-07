% main entry point for gamegerl simulator
-module(main).
-import(cpu, [tick/1]).
-export([start/0]).

-define(HELLO, 3).


start() ->
    Code = [16#f00d, 16#face, 16#b00f, 16#bee7, 16#c0d3],
    io:format("Hello ~w, world!\n", [?HELLO]),
    loop(Code, 0).

loop(Code, 10) ->
    io:fwrite("done!\n");
loop(Code, N) ->
    io:format("~w\n", [N]),
    [Head|Tail] = Code,
    cpu:tick(<<Head:16>>),
    loop(Tail ++ [Head], N+1).
