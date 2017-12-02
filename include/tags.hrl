
%%% Defines tags for use by the parser and encoder metadata, as well as what to do with it.

%%% List of tuples of tags.  Tuple elements have {regular expression for Header Tag|Ending Tag, record field to access}
-define(TAGS,[{"<sender>|</sender>", sender}, {"<dest>|</dest>", dest}, {"<msgID>|</msgID>", msgID}, {"<sequence>|</sequence>", sequence}]).
