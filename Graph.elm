module Graph where

import Helpers exposing (..)
import List exposing (..)

{-
- We also need an implementation of an undirected graph to represent the current
- gamestate:
- There is one node for each field
- Each edge from node v to w represents a pair of entangeled marks of one player
- the edge label is the number of the turn when the mark was set
-}


type alias NodeId =
  Int


type alias Node a =
  { id : NodeId
  , label : a
  }


type alias Edge e =
  { first : NodeId
  , second : NodeId
  , label : e
  }


type alias Graph a e =
  ( List (Node a), List (Edge e) )


getNodeFromId : Graph a e -> NodeId -> Maybe (Node a)
getNodeFromId graph nodeid =
  let
    ( nodes, edges ) =
      graph
  in
    head <| filter (\x -> x.id == nodeid) nodes


incidentEdges : Graph a e -> Node a -> List (Edge e)
incidentEdges graph { id } =
  let
    ( _, edges ) =
      graph

    f =
      \x -> x.first == id || x.second == id
  in
    List.filter f edges


adjacentNodes : Graph a e -> Node a -> List (Node a)
adjacentNodes graph node =
  let
    ( nodes, edges ) =
      graph
  in
    List.map unsafe
      <| filter isJust
      <| List.map (getNodeFromId graph)
      <| filter
          (\x ->
            if (x /= node.id) then
              True
            else
              False
          )
      <| removeDuplicates
      <| List.concatMap (\ed -> [ ed.first, ed.second ]) (incidentEdges graph node)

{- Checks via bfs if the graph contains a circle
- Nothing if no circle can be found
- One connected component that contains a cycle if there is one
-
- The algorithm uses the theorem:
- A connected undirected Graph contains a circle iff |E| >= |V|
-}

findComponentWithCycle : Graph a e -> Maybe (List (Node a))
findComponentWithCycle graph = let
    components = connectedComponents graph
    componentsWithCycle = filter (\c -> (length <| edgesInComponent graph c) >= (length c)) components
  in
    List.head componentsWithCycle

edgesInComponent : Graph a e -> List (Node a) -> List (Edge e)
edgesInComponent (nodes, edges) component = let
    nodeIds = List.map .id component
  in
    List.filter (\x -> (member x.first nodeIds) && (member x.second nodeIds)) edges 


{-Gives a list of a list of nodes, each of the smaller lists is a connected component of the graph-}
connectedComponents : Graph a e -> List (List (Node a))
connectedComponents graph = let
    (nodes, edges) = graph
  in
    case nodes of
        [] -> []
        (node::rest) -> let firstComp = reachableFromNode graph node in firstComp::(connectedComponents <| removeNodes graph firstComp)

removeNodes : Graph a e -> List (Node a) -> Graph a e
removeNodes graph toRemove = let
    (nodes, edges) = graph
  in
    case toRemove of
        [] -> graph
        (node::rest) -> removeNodes (List.filter (\x -> x.id /= node.id) nodes, List.filter (\x -> (x.first /= node.id) && (x.second /= node.id)) edges) rest        

isEdgeInGraph : Graph a e -> Edge e -> Bool
isEdgeInGraph graph edge = let
        (nodes, edges) = graph
  in
        member edge edges

removeEdge : Graph a e -> Edge e -> Graph a e
removeEdge graph edge = let
    (nodes,edges) = graph
  in
    (nodes, List.filter (\x -> x /= edge) edges)


{- Gives all nodes reachable frome one specific node -}

reachableFromNode : Graph a e -> Node a -> List (Node a)
reachableFromNode graph node =
  reachableFromNode' graph node []


reachableFromNode' : Graph a e -> Node a -> List (Node a) -> List (Node a)
reachableFromNode' graph node visited =
  if (List.member node visited) then
    visited
  else
    case List.filter (\x -> not (List.member x visited)) (adjacentNodes graph node) of
      [] ->
        (node :: visited)

      rest ->
        visitfold graph rest (node :: visited)


visitfold graph list visited =
  case list of
    [] ->
      visited

    n :: ns ->
      visitfold graph ns (reachableFromNode' graph n visited)

{- Wrapper to find all the nodes reachable from one connected component in the graph
- Observe that it is only necessary to call the reachableFromNode function for one node in the
- component, as "there exists a path from v to w" is transitive
-}

reachableFromComponent : Graph a e -> List (Node a) -> List (Node a)
reachableFromComponent graph nodes =
  case nodes of
    [] ->
      []

    n :: ns ->
      reachableFromNode graph n

{- Updates the node in the list with the right id to the new one -}

updateNode : Node a -> List (Node a) -> List (Node a)
updateNode node nodes =
  List.map
    (\x ->
      if x.id == node.id then
        node
      else
        x
    )
    nodes


