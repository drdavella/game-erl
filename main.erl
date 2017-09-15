% main entry point for gamegerl simulator
-module(main).
-import(rom, [read/1]).
-import(cpu, [run/1]).
-export([start/0]).


start() ->
    Code = rom:read("demon2.dump"),
    cpu:run(Code).
