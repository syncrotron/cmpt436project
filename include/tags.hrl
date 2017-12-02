
%%% Defines tags for use by the parser and encoder metadata, as well as what to do with it.

%%% List of tuples of tags.  Tuple elements have {regular expression for Header Tag|Ending Tag, record field to access, type case, size}
-define(TAGS,[
{"<msgID>|</msgID>", id, binary, 0},
{"<sender>|</sender>", sender, tuple, 3},
{"<sequence>|</sequence>", sequence, tuple, 2},
{"<request>|</request>", request, binary, 0},
{"<ftype>|</ftype>", ftype, binary, 0},
{"<dest>|</dest>", destination, tuple, 3}
]).
