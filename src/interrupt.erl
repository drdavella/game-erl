-module(interrupt).

-ifdef(TEST).
-compile(export_all).
-else.
-export([disable_interrupts/1, process_interrupts/1]).
-endif.


disable_interrupts(State) ->
    dict:store(di_pending, 2, State).

handle_disable(State, 2) ->
    dict:store(di_pending, 1, State);
handle_disable(State, 1) ->
    dict:store(di_pending, false, dict:store(ime, 0, State));
handle_disable(State, false) ->
    State.


process_interrupts(State) ->
    Disable = dict:fetch(di_pending, State),
    handle_disable(State, Disable).
