-module(parser).
-include("../include/tags.hrl").
-include("../include/message.hrl").
-export([start/0, spawnParser/1, read_file_chunks/1,loop_through_file/5, rest_of_file/5, decode/2, parse/4, send_msghandler/1, field_num/1, get_type/2, set_field/3]).

start() ->
  % Read from folder for files
  {ok, FileNames} = file:list_dir_all("./binaryfiles/"),
  io:format("~s~n", [FileNames]),
  spawnParser(FileNames).

spawnParser([Head|Tail]) ->
  % Spawn parser processes here for each file
  Pid = spawn(parser, read_file_chunks, ["./binaryfiles/" ++ Head]),
  spawnParser(Tail);
spawnParser([]) ->
  ok.

%& Read from raw file one byte at a time.
read_file_chunks(FileName) ->
  io:format("~s~n", [FileName]),
  {ok,Message} = file:open(FileName,[read,binary]),
  loop_through_file(Message,<<>>,file:read(Message,1), <<>>, <<>>). % <<>> is bit syntax

%% Reached end of file from Result case, ValidData contains utf8 (part of the message may also be utf8 unless we double encode), Data may contain the message.
loop_through_file(_,<<>>,eof, ValidData, Data) ->
  decode(ValidData,Data);

%% If end of file and Rest of data is given for 2nd part of tuple.
loop_through_file(_,Rest,eof, ValidData, Data) ->
  decode(ValidData,  <<Data/binary,Rest/binary>>);  % Append the rest to data and give it to decode function.

%% Loops through the binary to extract valid data (utf-8 containing metadata and possibly message data) + message data, unicode:characters_to_binary returns a result or
%% Tuple with error: binary() | {error, binary(), RestData} | {incomplete, binary(), binary()}
loop_through_file(Message,Acc,{ok,Bin}, ValidData, Data) when is_binary(Bin) ->
  % Unicode is an alias for utf8
  % Prepend Acc which is the previous loop's binary from the incomplete tuple case.
  % NOTE: They are not really appended but the function accepts it in the form of a "deep list".
  case unicode:characters_to_binary([Acc,Bin], unicode, unicode) of
    {error,_,Rest} ->         % Illegal unicode/latin1 characters, this should only exist in the data and not metadata, so append it to data.
      loop_through_file(Message,<<>>,file:read(Message,1), ValidData, <<Data/binary,Rest/binary>>);
	  {incomplete,_,Rest} ->    % Too few bytes to decode character, prepend to next byte and try again.
	    loop_through_file(Message,Rest,file:read(Message,1), ValidData, Data);
	  Result when is_binary(Result) ->
      MetadataEnd = list_to_binary("</metadata>"),
      Length = erlang:byte_size(MetadataEnd),
      Binary = <<ValidData/binary, Result/binary>>, % If is a utf8 character, append to the valid data.
      case erlang:byte_size(Binary) >= Length of    % Check that it is long enough to contain the metadata end tag.
        true ->
          % Check if reached </metadata> tag.
          case binary:match(Binary, MetadataEnd, [{scope, {erlang:byte_size(Binary), -Length}}]) of % Starts at end - length of the ValidData for a match of the tag.
            {_,_} ->
              % Found match and reached end of metadata.
              rest_of_file(Message,<<>>,file:read(Message,1024), Binary, Data);
            nomatch ->
              % Keep going.
              loop_through_file(Message,<<>>,file:read(Message,1), Binary, Data)
          end;
        false ->
            % Keep going.
            loop_through_file(Message,<<>>,file:read(Message,1), Binary, Data)
      end
  end.


%% End of metadata was reached, so read the rest of the file until EOF and append it to Data.
rest_of_file(_,_,eof, MetaData, Data) ->
  decode(MetaData, Data);
rest_of_file(Message, _, {ok,Bin}, MetaData, Data) when is_binary(Bin) ->
  NewData = <<Data/binary, Bin/binary>>,
  rest_of_file(Message, <<>>,file:read(Message,1), MetaData, NewData).


%% https://dzone.com/articles/erlang-binaries-and-bitstrings
%% erlang.org/euc/07/papers/1700Gustafsson.pdf  - introduces bit strings and BIFs for them.
%% erlang.org/documentation/doc-5.6/doc/programming_examples/bit_syntax.html
%% http://learnyousomeerlang.com/starting-out-for-real#binary-comprehensions
%% http://user.it.uu.se/~pergu/papers/erlang05.pdf

%% ValidData and Data are binary/bit string. NOTE: if number of bits in a bit string is divisible by 8, it is also a binary.
%% ValidData MUST be in UTF-8 to contain metadata and possibly the message/data.  ValidData may contain data if there was no metadata end tag.
decode(ValidData, Data) ->
  ValidDataSize = erlang:bit_size(ValidData),
  DataSize = erlang:bit_size(Data),
  io:format("PID ~w -- Number of bits in metadata: ~w~n", [self(), ValidDataSize]),
  io:format("PID ~w -- Number of bits in data: ~w~n", [self(), DataSize]),

  % Decode binary
  case unicode:characters_to_list(ValidData, unicode) of % unicode is an alias for utf8
    {error,_,_} ->
      io:format("PID ~w -- Something went wrong with ValidData extraction.", [self()]),
      false;
    {incomplete,_,_} ->
      io:format("PID ~w -- Something went wrong with ValidData extraction.", [self()]),
      false;
    Decoded ->
      io:format("PID ~w -- String format of what we decoded - ~s~n", [self(), Decoded]),
      io:format("PID ~w -- List format of what we decoded - ~w~n", [self(), Decoded]),
      Record = #message{},
      io:format("PID ~w --Initial Record - ~w~n", [self(), Record]),
      parse(Decoded, Data, Record, ?TAGS)
  end.


%%% Recursively performs function on each tag as defined by ?TAG list.
%%% Once done with all tags, add Data to record and send to message handler.
parse(_, Data, Record,[]) ->
  UpdatedRecord = Record#message{body = Data},
  send_msghandler(UpdatedRecord);
parse(MetaData, Data, Record, [Head | Tail]) ->
  TagsRegEx = element(1,Head),
  % Maybe check if the tags exist before splitting the string.
  IsolateData = re:split(MetaData,TagsRegEx,[{return,list},{parts,3}]),    % Returns a list with 3 parts [before head tag, data between tags, after end tag] with the tags removed.
  case (length(IsolateData) >= 2) of
    true ->
      FieldData = get_type(Head, lists:nth(2, IsolateData)),                     % Get the 2nd part of the list containing the data
      RecordField = element(2, Head),
      % TODO a field cannot be accessed through a variable.
      % TODO Maybe I can define the entire function as a macro?
      % http://erlang.org/pipermail/erlang-questions/2013-February/072406.html
      UpdatedRecord = set_field(RecordField, FieldData, Record),
      %UpdatedRecord = Record#message{UpdatedRecord = FieldData},           % Update the record's appropriate field.
      parse(MetaData, Data, UpdatedRecord, Tail);                           % Recurse
    false ->
      % Nothing in the metadata. Keep going.
      parse(MetaData, Data, Record, Tail)
  end.

% https://stackoverflow.com/questions/10821930/erlang-dynamic-record-editing
field_num(Field) ->
  Fields = record_info(fields, message),
  DifField = fun (FieldName) -> Field /= FieldName end,
  case length(lists:takewhile(DifField, Fields)) of
    Length when Length =:= length(Fields) ->
      {error, not_found};
    Length ->
      Length + 2
  end.

set_field(Field, Value, Record) ->
  setelement(field_num(Field), Record, Value).

%% Gets the appropriate data based on type in the tag definition.
get_type(TagDef, FieldData) ->
  case element(3,TagDef) of
    binary ->
      unicode:characters_to_binary(FieldData);
    tuple ->
      {ok, Tokens, _} = erl_scan:string(FieldData ++ "."),
      {ok, Tuple} = erl_parse:parse_term(Tokens),
      Tuple
  end.

send_msghandler(Record) ->
  io:format("PID ~w -- Populated Record - ~w~n", [self(), Record]),
  % Message pass to message handler the record.
  % MessageHandlerPID ! Record,
  Record.
