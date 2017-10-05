-module(game_erl).
-behavior(application).
-import(rom, [read/1]).
-import(cpu, [run/1]).
-export([start/2, stop/1]).

start(normal, _Args) ->
    Code = rom:read("demon2.dump"),
    cpu:run(Code).

stop(_State) ->
    ok.
