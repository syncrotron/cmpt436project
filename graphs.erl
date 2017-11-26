-module(graphs).
-export([practice/0]).
-export([start/0]).
-export([test/0]).

-record(message, {id, position, sequence, destination, body}).

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



labelsAndEdgesAsList(Graph,[])->
  empty;

labelsAndEdgesAsList(Graph, [H|RestofEdges])->


  L = getEdgeLabel(Graph,H),
  {Label,Edge} = {L, H},
  NewElement = [{Label,Edge}],
  labelsAndEdgesAsList(Graph, RestofEdges, NewElement)
  .

labelsAndEdgesAsList(Graph,[H|T], List) ->
  io:format("Labels and Edges: ~w~n",[List]),


  L = getEdgeLabel(Graph,H),
  {Label,Edge} = {L, H},
  NewElement = [{Label,Edge}],
  labelsAndEdgesAsList(Graph, T, List++NewElement)
  ;

labelsAndEdgesAsList(Graph,[], List) ->
  lists:sort(List)
  .


checkForTarget([],Target)->
  %didn't find target
  not_found
  ;
checkForTarget([H|T],Target) ->
  %io:format("Check for target~s~n",[H]),
  case H == Target of
    %We've found the target
    true -> H;
    %Keep checking
    false -> checkForTarget(T,Target)
  end
  .

greedySearch(G,StartV, Target) ->
  NAedges = digraph:in_edges(G,StartV),

  greedySearch(G,StartV,Target,NAedges)
  .

greedySearch(G, V,Target, DontUseEdges) ->



  Neighbours = digraph:out_neighbours(G,V),
  %NonVisited = Neighbours--VisitedNodes,
  X = checkForTarget(Neighbours, Target),

  case X of
    Target->
      {found,X};

    not_found->
      %OutEdges = digraph:out_edges(G,V),
      %MinEdge = minimumEdge(G, OutEdges),
      OutEdges = digraph:out_edges(G,V),
      InEdges = digraph:in_edges(G,V),
      UseEdges = OutEdges--DontUseEdges,
      Sorted = labelsAndEdgesAsList(G,UseEdges),
      case Sorted of
        empty->
          not_found;
        _->
          {Labels,Edges} = lists:unzip(Sorted),
          case forEachEdge(G,UseEdges,Target, DontUseEdges++InEdges) of
            {found,SendtoV}->
              SendtoV;
            not_found->
              not_found
          end
        end
      %V2 = getVertex(G, MinEdge),
      %greedySearch(G, V2, Target,VisitedNodes++V)
    end

  .

forEachEdge(G,[],Target, DontUseEdges)->
  not_found
  ;

forEachEdge(G,[Edge|TheRest],Target, DontUseEdges)->
  V = getVertex(G,Edge),
  %case lists:member(V,VisitedNodes) of
  %  true->
      %dont look at vertices already visited
  %    println("Hi"),
  %    forEachEdge(G,TheRest, Target, VisitedNodes);
  %  false->
      case greedySearch(G,V,Target,DontUseEdges) of
        {found,X} ->
          SendtoV = V,
          {found,SendtoV};
        not_found->
          forEachEdge(G,TheRest, Target, DontUseEdges)
        end
  %end

  .



test()->
  %Sattelites = [{SatName, Coords}]
  Satellites = [{"Mars1",{{x,32},{y,40},["Saturn1","Sun1"]}}, {"Saturn1",{{x,50},{y,70}},["Mars1","Sun1","Pluto1"]},  {"Sun1",{{x,5},{y,5}},["Mars1","Saturn1"]}],

  Earth1 = {"Earth1", {{x,32},{y,25}}},
  Graph = makeGraph(Earth1),
  %
  Mars1 = {"Mars1",{{x,32},{y,40}},[{"Sun1",{{x,5},{y,5}},[{"Mercury1",{{x,15},{y,15}},[]}]}]},

  addVertex(Graph,"Earth1", Mars1),
  io:format("Testing graph:",[]),
  io:format("GetVertexCoords testMars1: ~w~n", [getVertexCoords(Graph,"Mars1")]),
  io:format("Edges Earth, : ~w~n", [digraph:out_edges(Graph,"Earth1")]),
  io:format("Edges Mars1, : ~w~n", [digraph:out_edges(Graph,"Mars1")]),
  io:format("Edges Sun1, : ~w~n", [digraph:out_edges(Graph,"Sun1")]),
  io:format("Edges Mercury1, : ~w~n", [digraph:out_edges(Graph,"Mercury1")]),
  greedySearch(Graph,"Earth1","Fun1")

  .

forEachIKnow(G,I,[H|R])->
  addVertex(G,I,H),
  forEachIKnow(G,I,R)
  ;

forEachIKnow(G,I,[])->
    true
    .

addVertex(Graph, V0Name,V1)->
  io:format("V1: ~w~n,",[V1]),
  {VertexName, Coord,Knows } = V1,

  %Check if vertex in graph
  case digraph:vertex(Graph,VertexName) of
    %Add new vertex as we havent seen it before
    false ->
        digraph:add_vertex(Graph, VertexName, Coord);
    %Vertex already exists
    {V,Label} ->
      ok
  end,

  %calc distance
  Dist =calcEuclidean(Graph,V0Name,VertexName),
  addEdge(Graph, V0Name, VertexName, Dist),

  %add everything else I know to graph
  case Knows of
    []->
      println(VertexName++"knows no others");

    _->
      println(VertexName++" knows someone"),
      forEachIKnow(Graph,VertexName,Knows)

  end

  .

makeGraph(Root)->
  G = digraph:new(),
  {VertexName, Coord} = Root,
  io:format("~w~n",[Coord]),
  digraph:add_vertex(G,VertexName,Coord),

  G.



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
