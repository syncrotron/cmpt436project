%%% Created by Fredrik Johansson

%%% message record:
%%% KEY: id: 64 bit unique address, reminicent of IP address to identify objects universally
%%%      sender: a 3d euclidean tuple, consisting of {x, y, z}.
%%%      sequence: consiting of {number, total} where number is the number of the sequence
%%%                and total is the total amount of the sequence.
%%%      request: sequential message ID.
%%%      ftype: file type.
%%%      destination: a 3d euclidean tuple, consisting of {x, y, z}.
%%%      body: arbitrary binary
%%%      (mLength): maybe later
-record(message, {id = <<>>, sender = {0,0,0}, sequence = {0,0}, request= <<>>, ftype = <<>>, destination = {0,0,0}, body = <<>>}).
