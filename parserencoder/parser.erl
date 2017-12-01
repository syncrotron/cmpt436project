-module(parser).
-include("../include/tags.hrl").
-export([read_file_chunk/1,loop_through_file/3, decode/1, parse/1, send_msghandler/0]).

%& Read from raw file
read_file_chunk(FileName) ->
 {ok,Data} = file:open(FileName,[read,binary]),
 loop_through_file(Data,<<>>,file:read(Data,1024)). % <<>> is bit syntax

%% Reached end of file from Result case, confirmed utf8.
loop_through_file(_,<<>>,eof) ->
  utf8;
%% If end of file and Rest is given for 2nd part of tuple, assume latin1.
loop_through_file(_,_,eof) ->
  latin1;
%% Loops through the binary, unicode:characters_to_binary returns a result or
%% tuple with error: binary() | {error, binary(), RestData} | {incomplete, binary(), binary()}
loop_through_file(Data,Acc,{ok,Bin}) when is_binary(Bin) ->
  case unicode:characters_to_binary([Acc,Bin]) of   % Prepend Acc which is the previous loop's binary from the incomplete tuple case.
    {error,_,_} ->
      latin1;
	  {incomplete,_,Rest} ->
	    loop_through_file(Data,Rest,file:read(Data,1024));
	  Result when is_binary(Result) ->
	    loop_through_file(Data,<<>>,file:read(Data,1024))
  end.

%% https://dzone.com/articles/erlang-binaries-and-bitstrings
%% erlang.org/euc/07/papers/1700Gustafsson.pdf  - introduces bit strings and BIFs for them.
%% erlang.org/documentation/doc-5.6/doc/programming_examples/bit_syntax.html
decode(Bitstring) ->
  % if number of bits in bitstring is divisible by 8, it is also a binary.
  % erlang:bit_size(Bitstring),

  % Decode binary
  case unicode:characters_to_list(Bitstring, unicode) of % unicode is an alias for utf8
    {_,_,_} ->
      % Something went wrong.
      false;
    Data ->
      parse(Data)
  end.


parse(Data) ->
  % Parse metadata
  ok.


send_msghandler() ->
  % Message pass to message handler.
  ok.
