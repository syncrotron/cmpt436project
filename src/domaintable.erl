%%%Created by Sam Horovatin

-module(domaintable).
-include("../include/message.hrl").
-include("../include/domaintable.hrl").
-export([init/0,start/0]).
-export([test/0]).
-export([insert_object/1,remove_object/1,calcEuclidean/2,selectTimedOutObjs/0]).
-export([handleRequests/0,oldObjectRemover/0]).

%%% Call from the master startup and make availabe to the message handler.
%%%Could also contact the message handler instead by just flipping receive and send.
start() ->
  Pid = self(),
  domaintable:init(),

  %%% Register self to make availalbe to Message handler
  io:format("Starting DomainTable~n",[]),
  HRPid = spawn(domaintable,handleRequests,[]),
  register(pidOfdomaintable, HRPid),
  spawn(domaintable,oldObjectRemover,[]),
  %return HandleRequest pid
  {ok, HRPid}.

%% @doc Handles requests from the message handler
handleRequests() ->

  receive
    {insert_object,Object} ->
      io:format("recieved add to table~n",[]),
      insert_object(Object);

    {route, Target, DontUse, MessageId}->
      io:format("Recieved route request. Message id is:~w~n ",[MessageId]),
      Result = route(Target, DontUse),
      writeSendToFile(Result,MessageId)

      %MhPid ! Result

  end,
  handleRequests().

writeSendToFile(ObjId,MessageId)->
  StrObjId = erlang:binary_to_list(ObjId),
  %StrObjId = integer_to_list(Id),
  FileID = unicode:characters_to_list(MessageId),
  Directory = "D:\\CMPT 436 Project\\cmpt436project\\sendMessages\\",
  filelib:ensure_dir(Directory),
  FilePath = Directory++"dest",
  io:format("Writing too ~s~n",[FilePath]),
  file:write_file(FilePath, io_lib:fwrite("~p.\n",[StrObjId]))

  .

%%% init: intizalizes table on system boot
init() ->
  case filelib:is_dir(?DomaintableDB) of
    true->
      io:format("Domaintable already exists~n",[]),
      application:set_env(mnesia, dir, ?DomaintableDB),
      mnesia:create_schema([node()]),
      mnesia:start();



    false->
       %%%Sets directory to save database in
       io:format("Creating domaintable~n",[]),


    ok,_ =  mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(domaintable,
                      [{attributes, record_info(fields, object)},
                       %You dont have to specify index in our case as mnesia uses first field of record for inde by default.
                       %{index, [#object.id]}]).
                       {record_name, object},

                       {disc_copies, [node()]},       %%%Stores a copies on RAM and on disc
                       {type,set}])
    end.
%%%Test ops made by Nick%%%
%%%Should be transfered to the testing place once described.

test()->
  application:set_env(mnesia, dir, ?DomaintableDB),
  Mars1 = #object{id = 1,position = {23,50,4},delta_pos =x},
  Saturn1 = #object{id = 2, position = {100,-4, 70}, delta_pos =x},
  insert_object(Mars1),
  insert_object(Saturn1),
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
  io:format("ALlobjects: ~w~n",[AllObjects]),
  %remove_object(1),
  Target =  #object{id = 10, position ={200,30,18}, delta_pos = x},
  sortedSmallToTarget(Target),

  {_,OldObjs} = selectTimedOutObjs(),
  io:format("OldObjss: ~w~n",[OldObjs])
  %removeEach(OldObjs),
  %getAll()

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

%%%Select the objects that havent talked to me in a while -- 5mins right now
selectTimedOutObjs()->
  TimeNow= erlang:now(),
  io:format("Time is~w~n",[TimeNow]),
  {NowMega, NowSeconds, NowMicro} = TimeNow,
  Trans = fun()->
    MatchHead = #object{id='$1', last_msg_t_stamp={'$2','$3','_'},_='_'},

    %5Mins is 300 seconds. So search for stuff that has lasted for longer than 5 minutes ago.
    Gaurd = [{'=<','$2',NowMega},{'or',{'<','$3',NowSeconds-300}}],
    Result='$1',
    mnesia:select(domaintable,[{MatchHead,Gaurd,[Result]}])
    %MS = ets:fun2ms(fun(#object{id=ID,last_msg_t_stamp={Mega,Seconds_}}))
  end,
  mnesia:transaction(Trans).

removeEach([])->
  ok;

removeEach([ObjId|TheRest]) ->
  remove_object(ObjId),
  removeEach(TheRest)
  .


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
          -1;
        _->
          Available = Sorted --DontUse,
          case Available of
            []->
              %Try first again
              [SendTo|TheRest] = Sorted,
              {_, Obj} = SendTo,
              {_,Id,_,_,_} = Obj,
              Id;
            _->
              [SendTo|TheRest] = Available,
              {_, Obj} = SendTo,
              {_,Id,_,_,_} = Obj,
              Id
            end
      end

  end.

oldObjectRemover()->
  {_,OldObjs} = selectTimedOutObjs(),
  case OldObjs of
    {no_exists,_}->
      ok;
    _->
      io:format("Old objs~w~n",[OldObjs]),
      removeEach(OldObjs)
    end,
  timer:sleep(timer:minutes(5)),
  oldObjectRemover()

  .
