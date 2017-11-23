%%%Created by Sam Horovatin

-module(domaintable).
-include("../src/message.hrl").
-include("../src/domaintable.hrl").
-export([init/0]).


%%% init: intizalizes table on system boot
init() ->
    application:set_env(mnesia, dir, ?DomaintableDB),   %%%Sets directory to save database in
    ok =  mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(domaintable,
                      [{attributes, record_info(fields, object)},
                       {index, [#object.id]},
                       {disc_copies, [node()]},       %%%Stores a copies on RAM and on disc
                       {type, bag}]).                 %%%Allows for multiple unique entries under 1 key. Managed by table handler





%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%


%%% insert_object: Adds an object to the table via transaction
%%%      Id: 64 bit unique address, reminicent of IP address to identify objects universally
%%%      Position: a 3d ecludian tuple, consisting of X, Y, and Z values
%%%      Delta_pos: a kelper problem equation that solves for future object position based on last know position and associatve time stamp
insert_object(Id, Position, Delta_pos) ->
    ObjTrans = fun() ->
                  mnesia:write(#object{
                    id=Id,
                    position=Position,
                    delta_pos=Delta_pos,
                    last_msg_t_stamp=erlang:timestamp()
                  })
               end,
    mnesia:activity(transaction, ObjTrans).

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
    ObjTrans = fun() -> mnesia:delete({domaintable, Id}) end,
    mnesia:activity(transaction, ObjTrans).
