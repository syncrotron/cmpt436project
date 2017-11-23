%%% Created by Fredrik Johansson

%%% message record:
%%% KEY: id: 64 bit unique address, reminicent of IP address to identify objects universally
%%%      position: a 3d euclidean tuple, consisting of {x, y, z}.
<<<<<<< HEAD
%%%      sequence: consiting of {number, total} where number is the number of the sequence 
=======
%%%      sequence: consiting of {number, total} where number is the number of the sequence
>>>>>>> refs/remotes/origin/DomainTable
%%%                and total is the total amount of the sequence.
%%%      destination: a 3d euclidean tuple, consisting of {x, y, z}.
%%%      body: arbitrary binary
%%%      (mLenght): maybe later
<<<<<<< HEAD
-record(message, {id, position, sequence, destination, body}).
=======
-record(message, {id, position, sequence, destination, body}).
>>>>>>> refs/remotes/origin/DomainTable
