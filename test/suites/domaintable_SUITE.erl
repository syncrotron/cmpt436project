-module(domaintable_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../include/message.hrl").
-include("../include/domaintable.hrl").
-export([all/0]).
-export([ insert_object_test/1,
          object_by_Id_test/1,
          object_by_position/1,
          remove_object_test/1,
          calcEuclidean_test/1]).

all() -> [insert_object_test,remove_object_test,calcEuclidean_test].


insert_object_test(_Config)->

  application:set_env(mnesia, dir, ?DomaintableDB),
  mnesia:start(),
  Mars1 = #object{id = 0,position = {23,50,4},delta_pos =x},
  {_,Result} =domaintable:insert_object(Mars1),
  checkResult(Result),
  ct:log("insert_object_test Passed").

object_by_Id_test(_Config)->
 ok.
object_by_position(_Config)->
  ok.

remove_object_test(_Config)->
  application:set_env(mnesia, dir, ?DomaintableDB),

  {_,Result}= domaintable:remove_object(0),
  checkResult(Result),
  ct:log("remove_object_test Passed")
  .

calcEuclidean_test(_Config)->
  Mars1 = #object{id = 0,position = {23,50,4},delta_pos =x},
  Saturn1 = #object{id = 2, position = {100,-4, 70}, delta_pos =x},
  Euclrounded = round(domaintable:calcEuclidean(Mars1, Saturn1)),
  case Euclrounded of
    115->
      ok;
    _Else->
      ct:fail("Euclidean distance calculator is wrong")
    end,
  ct:log("Euclidean Distance is  ~w",[Euclrounded])

  .


checkResult(Result)->
  case Result of
    ok->
      ok;
    _->
      Reason = Result,
      ct:fail(Reason)
    end.
