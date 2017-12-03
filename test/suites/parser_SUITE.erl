-module(parser_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../include/message.hrl").
-export([all/0, parse_valid_test/1, compare_records/2]).


all() ->
  [parse_valid_test].

parse_valid_test(_Config) ->
  file:write_file("valid_file", unicode:characters_to_binary("<sourceid>" ++ <<4:8, 0:56>> ++ "<sourceid>" ++
                                                              "<sourceposition>{7,1,0}</sourceposition>" ++
                                                              "<senderid>" ++ <<3:8, 0:56>> ++ "</senderid>" ++
                                                              "<senderposition>{5,-9,0}</senderposition>" ++
                                                              "<sequence>{1,1}</sequence>" ++
                                                              "<request>1</request>" ++
                                                              "<ftype>txt</ftype>" ++
                                                              "<destination>{0,0,0}</destination>" ++
                                                              "</metadata>Hello")),
  Record = parser:read_file_chunks("valid_file"),
  ExpectedRecord = #message{sourceid = <<4:8, 0:56>>,
                            sourceposition = unicode:characters_to_binary("{7,1,0}"),
                            senderid = <<3:8, 0:56>>,
                            senderposition = unicode:characters_to_binary("{5,-9,5}"),
                            sequence = unicode:characters_to_binary("{1,1}"),
                            request = unicode:characters_to_binary("1"),
                            ftype = unicode:characters_to_binary("txt"),
                            destination = unicode:characters_to_binary("{0,0,0}"),
                            body = unicode:characters_to_binary("Hello")
                            },
  ResultList = compare_records(Record, ExpectedRecord),
  case lists:foldl(fun(Elem, AccIn) -> Elem == AccIn end, true, ResultList) of
    true ->
      ct:log("Passed valid test."),
      ok;
    false ->
      ct:fail(ResultList)
  end.

compare_records(Record, ExpectedRecord) ->
  ResultList = [],
  case Record#message.sourceid == ExpectedRecord#message.sourceid of
    true ->
      ResultList1 = lists:append(ResultList, [true]);
    false ->
      ResultList1 = lists:append(ResultList, [false])
  end,
  case Record#message.sourceposition == ExpectedRecord#message.sourceposition of
    true ->
      ResultList2 = lists:append(ResultList1, [true]);
    false ->
      ResultList2 = lists:append(ResultList1, [false])
  end,
  case Record#message.senderid == ExpectedRecord#message.senderid of
    true ->
      ResultList3 = lists:append(ResultList2, [true]);
    false ->
      ResultList3 = lists:append(ResultList2, [false])
  end,
  case Record#message.senderposition == ExpectedRecord#message.senderposition of
    true ->
      ResultList4 = lists:append(ResultList3, [true]);
    false ->
      ResultList4 = lists:append(ResultList3, [false])
  end,
  case Record#message.sequence == ExpectedRecord#message.sequence of
    true ->
      ResultList5 = lists:append(ResultList4, [true]);
    false ->
      ResultList5 = lists:append(ResultList4, [false])
  end,
  case Record#message.request == ExpectedRecord#message.request of
    true ->
      ResultList6 = lists:append(ResultList5, [true]);
    false ->
      ResultList6 = lists:append(ResultList5, [false])
  end,
  case Record#message.ftype == ExpectedRecord#message.ftype of
    true ->
      ResultList7 = lists:append(ResultList6, [true]);
    false ->
      ResultList7 = lists:append(ResultList6, [false])
  end,
  case Record#message.destination == ExpectedRecord#message.destination of
    true ->
      ResultList8 = lists:append(ResultList7, [true]);
    false ->
      ResultList8 = lists:append(ResultList7, [false])
  end,
  case Record#message.body == ExpectedRecord#message.body of
    true ->
      ResultList9 = lists:append(ResultList8, [true]);
    false ->
      ResultList9 = lists:append(ResultList8, [false])
  end,
  ResultList9.
