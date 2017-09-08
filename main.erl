% main entry point for gamegerl simulator
-module(main).
-import(rom, [read/1]).
-import(cpu, [tick/1]).
-export([start/0]).


start() ->
    Code = rom:read("demon2.dump"),
    loop(Code, 0).

loop(Code, 10) ->
    io:fwrite("done!\n");
loop(Code, N) ->
    io:format("~w\n", [N]),
    [Head|Tail] = Code,
    cpu:tick(<<Head:16>>),
    loop(Tail ++ [Head], N+1).
