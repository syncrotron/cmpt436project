%%%Created by Sam Horovatin

-module(domaintable).
-include("../src/message.hrl").
-include("../src/domaintable.hrl").
-export([init/0]).

init() ->
    application:set_env(mnesia, dir, ?DomaintableDB),   %%%Sets directory to save database in
    ok =  mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(domaintable,
                      [{attributes, record_info(fields, object)},
                       {index, [#object.id]},
                       {disc_copies, [node()]},       %%%Stores a copies on RAM and on disc
                       {type, bag}]).                %%%Allows for multiple unique entries under 1 key



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
