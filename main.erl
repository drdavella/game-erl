% main entry point for gamegerl simulator
-module(main).
-import(rom, [read/1]).
-import(cpu, [tick/1]).
-export([start/0]).


start() ->
    Code = rom:read("demon2.dump"),
    loop(Code, 0).

loop(Code, Pc) ->
    loop(Code, cpu:tick(Code, Pc)).
