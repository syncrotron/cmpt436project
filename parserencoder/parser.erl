-module(parser).
-include("../include/tags.hrl").
-export([read_file_chunks/1,loop_through_file/5, rest_of_file/5, decode/2, parse/1, send_msghandler/0]).

%& Read from raw file one byte at a time.
read_file_chunks(FileName) ->
 {ok,Message} = file:open(FileName,[read,binary]),
 loop_through_file(Message,<<>>,file:read(Message,1), <<>>, <<>>). % <<>> is bit syntax

%% Reached end of file from Result case, ValidData contains utf8 (part of the message may also be utf8 unless we double encode), Data may contain the message.
loop_through_file(_,<<>>,eof, ValidData, Data) ->
  decode(ValidData,Data);
%% If end of file and Rest of data is given for 2nd part of tuple.
loop_through_file(_,Rest,eof, ValidData, Data) ->
  % Append the rest to data and give it to decode function.
  decode(ValidData,  <<Data/binary,Rest/binary>>);
%% Loops through the binary to extract valid data (utf-8 containing metadata and possibly message data) + message data, unicode:characters_to_binary returns a result or
%% Tuple with error: binary() | {error, binary(), RestData} | {incomplete, binary(), binary()}
loop_through_file(Message,Acc,{ok,Bin}, ValidData, Data) when is_binary(Bin) ->
  % Unicode is an alias for utf8
  % Prepend Acc which is the previous loop's binary from the incomplete tuple case.
  % NOTE: They are not really appended but the function accepts it in the form of a "deep list".
  case unicode:characters_to_binary([Acc,Bin], unicode, unicode) of
    {error,_,Rest} ->
      % Illegal unicode/latin1 characters, this should only exist in the data and not metadata, so append it to data.
      loop_through_file(Message,<<>>,file:read(Message,1), ValidData, <<Data/binary,Rest/binary>>);
	  {incomplete,_,Rest} ->
      % Too few bytes to decode character, prepend to next byte and try again.
	    loop_through_file(Message,Rest,file:read(Message,1), ValidData, Data);
	  Result when is_binary(Result) ->
      MetadataEnd = unicode:list_to_binary("</metadata>"),
      Length = erlang:byte_size(MetadataEnd),
      % If is a utf8 character, append to the valid data.
      Binary = <<ValidData/binary, Result/binary>>,
      % Check if reached </metadata> tag.
      case binary:match(Binary, MetadataEnd, {scope, {erlang:byte_size(Binary), -Length}}) of % Starts at end - length of the ValidData for a match of the tag.
        {_,_} ->
          % Found match and reached end of metadata.
          rest_of_file(Message,<<>>,file:read(Message,1024), Binary, Data);
        nomatch ->
          % Keep going.
          loop_through_file(Message,<<>>,file:read(Message,1), Binary, Data)
      end
  end.

%% End of metadata was reached, so read the rest of the file until EOF and append it to Data.
rest_of_file(_,_,eof, ValidData, Data) ->
  decode(ValidData, Data);
rest_of_file(Message, _, {ok,Bin}, ValidData, Data) when is_binary(Bin) ->
  Data = <<Data/binary, Bin/binary>>,
  rest_of_file(Message, <<>>,file:read(Message,1024), ValidData, Data).

%% https://dzone.com/articles/erlang-binaries-and-bitstrings
%% erlang.org/euc/07/papers/1700Gustafsson.pdf  - introduces bit strings and BIFs for them.
%% erlang.org/documentation/doc-5.6/doc/programming_examples/bit_syntax.html
%% http://learnyousomeerlang.com/starting-out-for-real#binary-comprehensions
%% http://user.it.uu.se/~pergu/papers/erlang05.pdf

%% ValidData and Data are binary/bit string. Note if number of bits in a bit string is divisible by 8, it is also a binary.
%% ValidData MUST be in UTF-8 to contain metadata and possibly the message/data.
decode(ValidData, Data) ->
  ValidDataSize = erlang:bit_size(ValidData),
  DataSize = erlang:bit_size(Data),
  io:format("Number of bits in metadata: ~w", [ValidDataSize]),
  io:format("Number of bits in data: ~w", [DataSize]),

  % Decode binary
  case unicode:characters_to_list(ValidData, unicode) of % unicode is an alias for utf8
    {error,_,_} ->
      io:format("Something went wrong with ValidData extraction."),
      false;
    {incomplete,_,_} ->
      io:format("Something went wrong with ValidData extraction."),
      false;
    Decoded ->
      io:format("~w", [Decoded]),
      parse(Decoded)
  end.


parse(Data) ->
  % Parse metadata
  ok.


send_msghandler() ->
  % Message pass to message handler.
  ok.
