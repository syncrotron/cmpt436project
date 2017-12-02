-module(encoder).
-include("../include/tags.hrl").
-include("../include/message.hrl").
-export([set_metadata/1, encode/2, send_message/2, write_message_to_file/2]).

%% Record - contains the message and information for metadata.
set_metadata(Record) ->
  io:format("Creating metadata from given record.~n",[]).

%% Record - contains the already encoded message and information for metadata.
%% Metadata - utf-8 metadata.
encode(Record, Metadata) ->
  io:format("Encoding message.~n",[]).
  % Convert metadata record to binary and prepend to the message wrapped around encoded <data> tags.

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
write_message_to_file(File, Bytes) ->
  case file:write_file(File, Bytes) of
    {error, Reason} ->
      io:format("Error: ~w", [Reason]);
    ok ->
      io:format("Message written to file.",[])
  end.
