-module(encoder_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../include/message.hrl").
-export([all/0, encode_test/1]).


all() ->
  [encode_test].


encode_test(_Config) ->
  Pid = spawn(encoder, start, []),
  TestRecord = #message{sourceid = <<4:8, 0:56>>,
                        sourceposition = unicode:characters_to_binary("{7,1,0}"),
                        senderid = <<3:8, 0:56>>,
                        senderposition = unicode:characters_to_binary("{5,-9,5}"),
                        sequence = unicode:characters_to_binary("{1,1}"),
                        request = unicode:characters_to_binary("1"),
                        ftype = unicode:characters_to_binary("txt"),
                        destination = unicode:characters_to_binary("{0,0,0}"),
                        body = unicode:characters_to_binary("Hello")
                        },
  SourceIDList = unicode:characters_to_list(<<4:8, 0:56>>),
  SenderIDList = unicode:characters_to_list(<<3:8, 0:56>>),
  ExpectedBinary = unicode:characters_to_binary("<sourceid>" ++ SourceIDList ++ "<sourceid>" ++
                                                "<sourceposition>{7,1,0}</sourceposition>" ++
                                                "<senderid>" ++ SenderIDList ++ "</senderid>" ++
                                                "<senderposition>{5,-9,0}</senderposition>" ++
                                                "<sequence>{1,1}</sequence>" ++
                                                "<request>1</request>" ++
                                                "<ftype>txt</ftype>" ++
                                                "<destination>{0,0,0}</destination>" ++
                                                "</metadata>Hello"),
  Pid ! TestRecord,
  FileID = unicode:characters_to_list(TestRecord#message.request),
  timer:sleep(1000),
  {ok, BinaryResult} = file:read_file("sendMessages"++FileID++"/message"),
  case BinaryResult == ExpectedBinary of
    true ->
      ct:log("Passed encoder test.");
    false ->
      ct:fail(BinaryResult)
  end.
