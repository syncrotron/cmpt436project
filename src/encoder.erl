-module(encoder).
-include("../include/tags.hrl").
-include("../include/message.hrl").
-export([start/0, set_metadata/3, field_num/1, get_field/2, encode/2, send_message/2, write_message_to_file/2]).

% Loop through listening for a message containing a Record.
start() ->
  receive
    Record ->
      spawn(encoder, set_metadata,[Record, ?TAGS, ""])
  end,
  start().

%% Record - contains the message and information for metadata.
%% [Head|Tail] - List of tags/record fields defined
%% Metadata - list of utf-8.
set_metadata(Record, [], Metadata) ->
  register(pidofencodermaster, self()),

  AppendMetaData = Metadata ++ "</metadata>",
  io:format("Created metadata from given record.~n",[]),
  encode(Record, AppendMetaData);
set_metadata(Record, [Head|Tail], Metadata) ->
  FieldName = element(2, Head),
  Data = get_field(FieldName, Record),
  Result = io_lib:format("~p",[Data]),
  StringData = lists:flatten(Result),      % convert Data to utf-8
  StringFieldName = atom_to_list(FieldName),
  AppendMetaData = Metadata ++ "<" ++ StringFieldName ++ ">" ++ StringData ++ "</" ++ StringFieldName ++ ">",
  set_metadata(Record, Tail, AppendMetaData).

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

get_field(Field, Record) ->
  element(field_num(Field), Record).

%% Record - contains the already encoded message and information for metadata.
%% Metadata - utf-8 metadata.
encode(Record, Metadata) ->
  io:format("Encoding message.~n",[]),
  % Convert metadata record to binary and prepend to the message wrapped around encoded <data> tags.
  EncodedMetadata = unicode:characters_to_binary(Metadata),
  Message = EncodedMetadata ++ Record#message.body,
  FileID = unicode:characters_to_list(Record#message.request),
  write_message_to_file(FileID, Message).

% If we use an IO device.
% IoDevice takes a pid, Bytes is the data we are sending.
send_message(IoDevice,Bytes) ->
  case file:write(IoDevice, Bytes) of
    {error, Reason} ->
      io:format("Error: ~w", [Reason]);
    ok ->
      io:format("Message written to IO device.",[])
  end.

% For testing.
% File is a file name, Bytes is the data.
write_message_to_file(FileID, Bytes) ->
  Directory = "sendMessages"++FileID++"/",
  filelib:ensure_dir(Directory),
  FilePath = Directory++"message",
  case file:write_file(FilePath, Bytes) of
    {error, Reason} ->
      io:format("Error: ~w", [Reason]);
    ok ->
      io:format("Message written to file.",[])
  end.
