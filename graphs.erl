-module(graphs).
-export([practice/0]).
-export([start/0]).
-export([test/0]).

println(String) ->
    io:format("~s~n",[String])
  .

getEdgeLabel(Graph,Edge) ->
  {_,_,_,Label} = digraph:edge(Graph,Edge),
  Label.
getVertex(Graph, Edge) ->
  {_,_,V2,_} = digraph:edge(Graph,Edge),
  V2.

start()->
  println("HI")
.

calcEuclidean(G, V0, V1)->
  X0 = getXCoord(G, V0),
  X1 = getXCoord(G,V1),
  Y0 = getYCoord(G, V0),
  Y1 = getYCoord(G,V1),
  io:format("X0 is: ~w~n", [X0]),
  io:format("X1 is: ~w~n", [X1]),
  io:format("Y0 is: ~w~n", [Y0]),
  io:format("Y1 is: ~w~n", [Y1]),


  X = X1 - X0,
  Y = Y1 - Y0,
  %Z = Z1 - Z0,
  %math:sqrt((X*X)+ (Y*Y) + (Z*Z))

  math:sqrt((X*X)+ (Y*Y))
  .

minimumEdge(Graph, [H|T])->
  minimumEdge(Graph, H, T).

minimumEdge(Graph, MinEdge, [H|T])->
    Min = getEdgeLabel(Graph, MinEdge),
    Label = getEdgeLabel(Graph, H),
    case Min<Label of
      true -> minimumEdge(Graph, MinEdge,T);
      false -> minimumEdge(Graph, H , T)
    end;


minimumEdge(Graph, MinEdge,[])->
  MinEdge.

checkForTarget([],Target)->
  false
  ;
checkForTarget([H|T],Target) ->
  io:format("Check for target~s~n",[H]),
  case H == Target of
    true -> H;
    false -> checkForTarget(T,Target)
  end
  .

getLabelsAndEdges(Graph, [H|RestofEdges])->


  L = getEdgeLabel(Graph,H),
  {Label,Edge} = {L, H},
  NewElement = [{Label,Edge}],
  getLabelsAndEdges(Graph, RestofEdges, NewElement)
  .

getLabelsAndEdges(Graph,[H|T], List) ->
  io:format("Labels and Edges: ~w~n",[List]),


  L = getEdgeLabel(Graph,H),
  {Label,Edge} = {L, H},
  NewElement = [{Label,Edge}],
  getLabelsAndEdges(Graph, T, List++NewElement)
  ;

getLabelsAndEdges(Graph,[], List) ->
  lists:sort(List)
  .


greedySearch(G, V,Target) ->
  Neighbours = digraph:out_neighbours(G,V),

  X = checkForTarget(Neighbours, Target),
  println("Gothere"),
  case X == Target of
    true->X;
    false->
      OutEdges = digraph:out_edges(G,V),
      MinEdge = minimumEdge(G, OutEdges),
      V2 = getVertex(G, MinEdge),
      greedySearch(G, V2, Target)
    end

  .

test()->
  G = digraph:new(),
  Mars1 = digraph:add_vertex(G,"Mars1",{{x,32},{y,40}}),
  Saturn2 = digraph:add_vertex(G,"Saturn2",{{x,55},{y,45}}),
  Pluto1 = digraph:add_vertex(G,"Pluto1", {{x,100},{y,99}}),
  Sun1 = digraph:add_vertex(G, "Sun1",{{x,5},{y,5}}),
  {{x,X},{y,Y}} = getVertexCoords(G,"Mars1"),
  Yy = getYCoord(G,"Saturn2"),
  io:format("Coords: ~w~n",[X]),
  io:format("Y Saturn2 :~w~n ",[Yy]),

  EL0 = calcEuclidean(G,"Mars1","Saturn2"),
  {E,E1} = addEdge(G,"Mars1","Saturn2", EL0),
  El1 = calcEuclidean(G,Mars1,Pluto1),
  {FooEdge,_} = addEdge(G,"Mars1","Saturn2", El1),

  addEdge(G,Mars1,Sun1,calcEuclidean(G,Mars1,Sun1)),
  io:format("Edge Label: ~w~n",[getEdgeLabel(G,FooEdge)]),
  io:format("Edges Saturn2: ~w~n",[digraph:out_edges(G,"Saturn2")]),
  OutEdges = digraph:out_edges(G,Mars1),
  EdgesAndLabels = getLabelsAndEdges(G,OutEdges),
  io:format("getLabelsAndEdges test:~w~n",[EdgesAndLabels]),
  io:format("Min edge: ~w~n: ", [getEdgeLabel(G,minimumEdge(G, OutEdges))])
  %io:format("~w~n",[greedySearch(G, Mars1, f)])
  .

addEdge(G,V0,V1,Label)->
  E0 = digraph:add_edge(G,V0,V1,Label),
  E1 = digraph:add_edge(G,V1,V0,Label),
  {E0,E1}.

getVertexCoords(Graph,VertexName)->
   {_,{{x,X},{y,Y}}} = digraph:vertex(Graph,VertexName),
   {{x,X}, {y,Y}}.

getXCoord(Graph, VertexName)->
  {_,{{x,X},{_,_}}} = digraph:vertex(Graph,VertexName),
  X.

getYCoord(Graph,VertexName)->
  {_,{{_,_},{y,Y}}} = digraph:vertex(Graph,VertexName),
  Y.




practice() ->
  G = digraph:new(),
  V1 = digraph:add_vertex(G, a,"Vertex1"),
  digraph:add_vertex(G,"A"),
  {V, Label} = digraph:vertex(G,V1),
  println(Label),
  G2 = digraph_utils:subgraph(G,digraph:vertices(G)),
  List= digraph:vertices(G),
  io:format("~w~n",[List]),
  digraph:add_vertex(G2,b),

  digraph:add_edge(G2,a,b,50),
  digraph:add_vertex(G2,c),
  digraph:add_edge(G2,a,c,40),
  digraph:add_edge(G2,b,c,20),
  List2 = digraph:vertices(G2),

  io:format("Edges: ~w~n",[digraph:edges(G2)]),
  io:format("Vertices~w~n",[List2]),
  io:format("SP: ~w~n",[digraph:get_short_path(G2,a,c)]),
  L = digraph:out_edges(G2,a),

  io:format("Labeled edge~w~n ",[L]),
  [T|TR] = L,

  io:format("Labeled edge~w~n ",[T]),
{Edge, V5,V6, Lt} = digraph:edge(G2,T),
  io:format("Labeled egdge~w~n",[getEdgeLabel(G2,T)])
  %io:format("Edge A ~w~n",[] )
  .
