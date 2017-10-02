-module(memory).
-import(utils, [update_tick/2]).
-export([load/3, load_imm/3, load_imm_d/3]).


get_hl(State) ->
    (dict:fetch(h, State) bsl 8) bor dict:fetch(l, State).

read_mem(State, Addr) ->
    array:get(Addr, dict:fetch(mem, State)).

write_mem(State, Addr, Value) ->
    erlang:error(not_implemented),
    State.


load(State, Dest, Source) when Source == hl ->
    Address = get_hl(State),
    io:fwrite("read memory at addr=~w~n", [Address]),
    NewState = dict:store(Dest, read_mem(State, Address), State),
    update_tick(8, NewState);
load(State, Dest, Source) when Dest == hl ->
    io:fwrite("load memory from ~w!~n", [Source]),
    NewState = write_mem(State, get_hl(State), 0),
    update_tick(8, NewState);
load(State, Dest, Source) ->
    io:fwrite("ld ~w->~w~n", [Source, Dest]),
    NewState = dict:store(Dest, dict:fetch(Source, State), State),
    update_tick(4, NewState).

load_imm_dest(High, Low) ->
    Index = maps:get(Low, #{6=>0,16#e=>1}) + (High * 2) + 1,
    lists:nth(Index, [b,c,d,e,h,l,hl,a]).

% Load single immediate word to location in memory
load_imm_impl(hl, State, Data) ->
    Address = get_hl(State),
    io:fwrite("mem[0x~.16B] with 0x~.16B~n", [Address, Data]),
    NewState = write_mem(State, Address, Data),
    update_tick(12, NewState);
% Load single word immediate to a register
load_imm_impl(Dest, State, Data) ->
    io:fwrite("~w with 0x~.16B~n", [Dest, Data]),
    NewState = dict:store(Dest, Data, State),
    update_tick(8, NewState).

% Load a single word
load_imm(State, Code, [High, Low]) ->
    load_imm_impl(load_imm_dest(High, Low), State, hd(Code)).

load_imm_d_impl([H, L], State, High, Low) ->
    io:fwrite("load ~w~w with 0x~.16B~.16B~n", [H, L, High, Low]),
    NewState = dict:store(H, High, State),
    dict:store(L, Low, State);
load_imm_d_impl([sp], State, High, Low) ->
    io:fwrite("load sp with 0x~.16B~.16B~n", [High, Low]),
    Data = (High bsl 8) bor Low,
    dict:store(sp, Data, State).

% Load a double word
load_imm_d(State, Code, Index) ->
    [HighData, LowData | _] = Code,
    Dests = lists:nth(Index + 1, [[b,c], [d,e], [h,l], [sp]]),
    NewState = load_imm_d_impl(Dests, State, HighData, LowData),
    update_tick(8, NewState).
