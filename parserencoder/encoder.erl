-module(encoder).
-include("../include/tags.hrl").
-export([set_metadata/0, encode/1]).

set_metadata() ->
  io:format("Creating metadata from given record.~n",[]).

encode(message) ->
  io:format("Encoding message.~n",[]).
