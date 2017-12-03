%%%Created by Sam Horovatin

%%%----FILE tables.hrl----
%%% Contains record definition for use in the domain table

%%% object record:
%%% KEY: id: 64 bit unique address, reminicent of IP address to identify objects universally
%%%      position: a 3d ecludian tuple, consisting of X, Y, and Z values
%%%      delta_pos: a kelper problem equation that solves for future object position based on last know position and associatve time stamp
%%%      last_msg_t_stamp: a local time stamp of last update to object row data
-record(object, {id, position, delta_pos, last_msg_t_stamp}).

-define(DomaintableDB, "domaindb").
-define(TestDomaintableDB, "../test/testdomaindb").
