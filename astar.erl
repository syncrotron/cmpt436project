-module(astar).
-export([start/3]).
-record(node,{
  parent,
  x, y, z,
  f,g,h,
  known
}).


heuristic({location,{X0,Y0,Z0}}, {goalLocation,{X1,Y1,Z1}}) ->
  X = X1 - X0,
  Y = Y1 - Y0,
  Z = Z1 - Z0,

  math:sqrt((X*X)+ (Y*Y) + (Z*Z))
  .


start(Start, Goal, Graph) ->
  Open = [Start],

  [Pop|Open] = Open,
  list:foreach( , Pop#node.known)

  .

%test() ->
%  .
