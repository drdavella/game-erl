-module(rom).
-export([read/1]).


read(Filename) ->
    {ok, Rom} = file:read_file(Filename),
    N = 0,
    parse(Rom).

parse(<<Word:8, Rest/binary>>) ->
    [Word] ++ parse(Rest);
parse(_) ->
    [].
