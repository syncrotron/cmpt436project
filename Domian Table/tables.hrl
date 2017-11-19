%%%Created by Sam Horovatin

%%%----FILE tables.hrl----
%%% Contains record definitions for tables in use in domain table

%%% object record:
%%% KEY: id: 64 bit unique address, reminicent of IP address to identify objects universally
%%%      position: a 3d ecludian tuple, consisting of X, Y, and Z values
%%%      delta_pos: a kelper problem equation that solves for future object position based on last know position and associatve time stamp
%%%      last_msg_t_stamp: a local time stamp of last update to object row data
-record(object, {id, position, delta_pos, last_msg_t_stamp}).

%%% domain record:
%%% KEY: id: 32 bit unique address, reminicent of IP address to help identify domain groupings
%%%      inner_position_r: a 3d ecludian tuple, consisting of X, Y, and Z values representing the inner most range of orbital domain
%%%      outer_position_r: a 3d ecludian tuple, consisting of X, Y, and Z values representing the outer most range of orbital domain
%%%      num_members: number of member objects in domain
-record(domain, {id, inner_position_r, outer_position_r, num_members}).

%%% domain_member record:
%%% obj: The id key for an object in object table
%%% domain: The id key for a domain in object table
-record(domain_member, {obj, domain}).
