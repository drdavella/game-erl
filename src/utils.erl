-module(utils).
-export([inc16/1, dec16/1, update_tick/2]).

-define(WORD_MASK, 16#ffff).


inc16(Value) ->
    (Value + 1) band ?WORD_MASK.

dec16(Value) ->
    (Value + ?WORD_MASK) band ?WORD_MASK.

update_tick(Count, State) ->
    dict:update_counter(tick, Count, State).
