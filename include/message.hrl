%%% Created by Fredrik Johansson

%%% message record:
%%%
%%% KEY: sourceid: 64 bit unique address of the message source, reminicent of IP address to identify objects universally
%%%      sourceposition: a 3d euclidean tuple, consisting of {x, y, z} of the message sources position.
%%%      senderid: 64 bit unique address of last node to recieve and send the message, reminicent of IP address to identify objects universally
%%%      senderposition: a 3d euclidean tuple, consisting of {x, y, z} of last node to recieve and send the messages position.
%%%      sequence: consiting of {number, total} where number is the number of the sequence
%%%                and total is the total amount of the sequence.
%%%      request: a hexidecimal request number.
%%%      ftype: the file type for the body
%%%      destination: a 3d euclidean tuple, consisting of {x, y, z}.
%%%      body: arbitrary binary
%%%      (mLenght): maybe later
-record(message, {sourceid = <<>>,
                  sourceposition = {0,0,0},
                  senderid = <<>>,
                  senderposition = {0,0,0},
                  sequence = {0,0},
                  request = <<>>,
                  ftype = <<>>,
                  destination = {0,0,0},
                  body = <<>>}).
