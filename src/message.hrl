%%% Created by Fredrik Johansson

%%% message record:
%%% KEY: id: 64 bit unique address, reminicent of IP address to identify objects universally
%%%      position: a 3d ecludian tuple, consisting of {x, y, z}.
%%%      sequence: consiting of {number, total} where number is the number of the sequence 
%%%                and total is the total amount of the sequence.
%%%      destination: a 3d ecludian tuple, consisting of {x, y, z}.
%%%      (mLenght): maybe later
-record(message, {id, position, sequence, destination}).