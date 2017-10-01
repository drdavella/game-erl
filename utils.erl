-module(utils).
-export([update_tick/2]).


update_tick(Count, State) ->
    dict:update_counter(tick, Count, State).
