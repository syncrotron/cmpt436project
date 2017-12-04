
%%% Defines tags for use by the parser and encoder metadata, as well as what to do with it.

%%% List of tuples of tags.  Tuple elements have {regular expression for Header Tag|Ending Tag, record field to access, type case, size}
-define(TAGS,[
{"<sourceid>|</sourceid>", sourceid, binary, 0},
{"<sourceposition>|</sourceposition>", sourceposition, tuple, 3},
{"<senderid>|</senderid>", senderid, binary, 3},
{"<senderposition>|</senderposition>", senderposition, tuple, 3},
{"<sequence>|</sequence>", sequence, tuple, 2},
{"<request>|</request>", request, binary, 0},
{"<ftype>|</ftype>", ftype, binary, 0},
{"<destination>|</destination>", destination, tuple, 3}
]).
