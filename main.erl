% main entry point for gamegerl simulator
-module(main).
-export([start/0]).


start() ->
    io:fwrite("Hello, world!\n"),
    loop(0).

loop(10) ->
    io:fwrite("done!\n");
loop(N) ->
    io:format("~w\n", [N]),
    loop(N+1).
