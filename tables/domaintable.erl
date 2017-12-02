%%%Created by Sam Horovatin

-module(domaintable).
-include("../include/message.hrl").
-include("../include/domaintable.hrl").
-export([init/0,start/0]).
-export([test/0]).
-export([insert_object/1,remove_object/1,calcEuclidean/2,selectTimedOutObjs/0]).

%%% Call from the master startup and make availabe to the message handler.
%%%Could also contact the message handler instead by just flipping receive and send.
start() ->
  Pid = self(),
  %%% Register self to make availalbe to Message handler
  register(domaintable, self()),
  io:format("Establishing connection with Message Handler~n",[]),
  %Make Connection here.
  receive
    {connection, MhPid} ->
      %%%Give message handler back our pid
      MhPid ! {connection,Pid},
      handleRequests(MhPid)
  end
  .
%%% Handles requests from the message handler
handleRequests(MhPid) ->
  receive
    {insert_object,Object} ->
      {_,Result} = insert_object(Object),
      MhPid ! Result;
    {route, Target, DontUse}->
      Result = route(Target, DontUse),
      MhPid ! Result

  end,
handleRequests(MhPid)
.
%%% init: intizalizes table on system boot


init() ->
    application:set_env(mnesia, dir, ?DomaintableDB),   %%%Sets directory to save database in

    ok,_ =  mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(domaintable,
                      [{attributes, record_info(fields, object)},
                       %You dont have to specify index in our case as mnesia uses first field of record for inde by default.
                       %{index, [#object.id]}]).
                       {record_name, object},

                       {disc_copies, [node()]},       %%%Stores a copies on RAM and on disc
                       {type,set}]).%for testing.
                       %{type, bag}]).                 %%%Allows for multiple unique entries under 1 key. Managed by table handler

%%%Test ops made by Nick%%%
%%%Should be transfered to the testing place once described.

test()->
  application:set_env(mnesia, dir, ?DomaintableDB),
  Mars1 = #object{id = 1,position = {23,50,4},delta_pos =x},
  Saturn1 = #object{id = 2, position = {100,-4, 70}, delta_pos =x},
  %insert_object(Mars1),
  %insert_object(Saturn1),
  Objects = object_by_Id(1),
  [Obj|OtherObjects] = Objects,
  Marsid = Mars1#object.id,
  io:format("~p~n",[Obj]),
  case Obj#object.id of
    Marsid->
      Obj,
      io:format("Read passed test~n",[]);
    _Else->
      io:format("Read failed test~n",[])
  end,
  {_, AllObjects}= getAll(),
  %remove_object(1),
  %Target =  #object{id = 10, position ={200,30,18}, delta_pos = x},
  %sortedSmallToTarget(Target)

  selectTimedOutObjs()
  .


%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%%% insert_object/1: Helper function. Useful for defining the object beforehand rather than passing in seperate values
insert_object(Object)->
  insert_object(Object#object.id, Object#object.position, Object#object.delta_pos).

%%% insert_object: Adds an object to the table via transaction
%%%      Id: 64 bit unique address, reminicent of IP address to identify objects universally
%%%      Position: a 3d ecludian tuple, consisting of X, Y, and Z values
%%%      Delta_pos: a kelper problem equation that solves for future object position based on last know position and associatve time stamp
insert_object(Id, Position, Delta_pos) ->
    ObjTrans = fun() ->
                  mnesia:write(domaintable,
                  #object{
                    id=Id,
                    position=Position,
                    delta_pos=Delta_pos,
                    last_msg_t_stamp=erlang:timestamp()},
                    write),
                    ok
               end,
    %mnesia:activity(transaction, ObjTrans).
    mnesia:transaction(ObjTrans).

%%% object_by_Id: Returns a list of records matching the given ID
%%%      Id: 64 bit unique address, reminicent of IP address to identify objects universally
object_by_Id(Id) ->
    ObjTrans = fun() ->
                  mnesia:read({domaintable, Id})

               end,
    mnesia:activity(transaction, ObjTrans).

%%% object_by_position: Returns a lsit of records matching the given position
%%%      Position: a 3d ecludian tuple, consisting of X, Y, and Z values
object_by_position(Position) ->
    ObjTrans = fun() ->
                  mnesia:match_object(domaintable, {object, '_' , Position , '_' , '_' }, read)
               end,
    mnesia:activity(transaction, ObjTrans).

%%% object_by_Id: Deletes an entire object history from the table
%%%      Id: 64 bit unique address, reminicent of IP address to identify objects universally
remove_object(Id) ->
    ObjTrans = fun() ->
      mnesia:delete({domaintable, Id})
     end,
    mnesia:transaction(ObjTrans).

%%%getAll: Get all objects in domaintable
%%%returns all objects in domaintable as {atomic, [Objects]}
getAll() ->
  ObjTrans = fun() ->
    mnesia:select(domaintable,[{'_',[],['$_']}])
  end,
  mnesia:transaction(ObjTrans).


%%%sortedSmallEntries: Makes a sorted list of all entries in the table. Calculates distances of all to target and sorts from minimum through all accordingly and returns a list in the form [{DistFloat, Object}]
sortedSmallToTarget(Target)->
  GetAll = getAll(),
  case GetAll of
    []->
      [];
    _Else->
      {_,Objects} = GetAll,
      sortedSmallToTarget(Objects, Target, [])
    end
  .
sortedSmallToTarget(Objects,Target, List)->
  case Objects of
    []->
      lists:sort(List);
    _Else->
      %ID = Obj#object.id
      [Obj|Rest] = Objects,
      Dist = calcEuclidean(Obj, Target),
      Element = [{Dist, Obj}],
    sortedSmallToTarget(Rest, Target, List++Element)
    end
  .

%%%calcEuclidean: Calculate the euclidean distance between 2 objects
%%%Obj0 and Obj1 are object records defined in include/domaintable.hrl
%%%returns floating point Euclidean distance
calcEuclidean(Obj0, Obj1)->
  {X0,Y0,Z0} = Obj0#object.position,
  {X1,Y1,Z1} = Obj1#object.position,

  X = X1 - X0,
  Y = Y1 - Y0,
  Z = Z1-Z0,
  %Z = Z1 - Z0,
  %math:sqrt((X*X)+ (Y*Y) + (Z*Z))

  math:sqrt((X*X)+ (Y*Y) + (Z*Z))
  .
selectTimedOutObjs()->
  TimeNow= erlang:now(),
  io:format("Time is~w~n",[TimeNow]),
  {Mega, NowSeconds, Micro} = TimeNow,
  Trans = fun()->
    MatchHead = #object{id='$1', last_msg_t_stamp={'_','$2','_'},_='_'},

    %5Mins
    Gaurd = {'<','$2',NowSeconds-300},
    Result='$1',
    mnesia:select(domaintable,[{MatchHead,[Gaurd],[Result]}])
  end,
  mnesia:transaction(Trans).

route(Target, DontUse)->
  case Target of
    {id, ID}->
      Object = object_by_Id,
      case Object of
        []->
          nothing_in_domain;
        _->
          Object
        end;
    {position, Pos}->
      TargetObj = #object{position = Pos},
      Sorted = sortedSmallToTarget(TargetObj),
      case Sorted of
        []->
          nothing_in_domain;
        _->
          Available = Sorted --DontUse,
          case Available of
            []->
              no_available;
            _->
              Available
            end
      end

  end.

%oldObjectCollector()->
