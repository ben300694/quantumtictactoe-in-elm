module GameLogic (..) where

import List exposing (..)
import Maybe exposing (..)
import Debug exposing (crash)


removeDuplicates : List a -> List a
removeDuplicates list =
  case list of
    [] ->
      []

    l :: ls ->
      if (List.member l ls) then
        (removeDuplicates ls)
      else
        (l :: (removeDuplicates ls))


isJust : Maybe a -> Bool
isJust a =
  case a of
    Just _ ->
      True

    _ ->
      False


unsafe : Maybe a -> a
unsafe v =
  case v of
    Just x ->
      x

    Nothing ->
      Debug.crash "unsafe with Nothing"


type Player
  = O
  | X



{-
- Opposed to normal tictactoe both player can get three marks in
- a row in the same turn, so we have to add a new result, here called 'tie'
- which is different to 'draw' where no player has three marks in a row
-}


type Result
  = Draw
  | Tie Player
  | Winner Player



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
- A list of nodes of the circle (in the right order) and a list of edges (in the right order) in the circle
-}


findCycle : Graph a e -> Maybe ( List (Node a), List (Edge e) )
findCycle graph =
  Nothing



{- TODO -}
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



{- Each vertex contains three important labels:
- The position in the field it represents, the number of the move of which it became a classical mark, and the player who has
- a classical mark in this position (it is Nothing if neither of the players has a classical mark there)
-
- Each edge has two labels:
- The number of the move when a player inserted an entangeled pair there and to which player the marks belong
-}


type alias Position =
  { col : Int, row : Int }


type alias NodeLabel =
  { pos : Position, numberOfMove : Int, player : Maybe Player }


type alias EdgeLabel =
  { numberOfMove : Int, player : Player }


type alias Field =
  Graph NodeLabel EdgeLabel



{- There are two different types of moves that can occur:
- A player can set two new entangeled marks, this corresponds to adding a new edge in the game graph
-
- There is a situation where a collapse must occur, one of the players chooses where he wants to have the traditional mark
- All the other things follow from this move
-}


type Move
  = Entangled (Edge EdgeLabel)
  | Collapse (Node NodeLabel)


type alias Moves =
  List Move


type alias Round =
  Int


type GameState
  = FinishedGame Result Field Moves
  | NotFinishedGame Player Field Moves Round


returnField : GameState -> Field
returnField state =
  case state of
    FinishedGame _ field _ ->
      field

    NotFinishedGame _ field _ _ ->
      field


returnRound : GameState -> Round
returnRound state =
  case state of
    FinishedGame _ _ _ ->
      0

    NotFinishedGame _ _ _ n ->
      n


returnPlayer : GameState -> Maybe Player
returnPlayer state =
  case state of
    FinishedGame _ _ _ ->
      Nothing

    NotFinishedGame player _ _ _ ->
      Just player


returnMoves : GameState -> Moves
returnMoves state =
  case state of
    FinishedGame _ _ moves ->
      moves

    NotFinishedGame _ _ moves _ ->
      moves


switchPlayer : Player -> Player
switchPlayer p =
  case p of
    X ->
      O

    O ->
      X


nodeConstr : ( Int, ( Int, Int ) ) -> Node NodeLabel
nodeConstr ( i, ( x, y ) ) =
  { id = i, label = { pos = { col = x, row = y }, numberOfMove = -1, player = Nothing } }


zip : List a -> List b -> List ( a, b )
zip =
  List.map2 (,)


tuples : List a -> List b -> List ( a, b )
tuples xs ys =
  List.concatMap (\x -> (List.map (\y -> ( x, y )) ys)) xs


fieldNumbers =
  tuples [1..3] [1..3]


nodes =
  List.map nodeConstr (zip [1..9] fieldNumbers)



{- Represents the beginning of the game, Player X always starts -}


emptyField : Field
emptyField =
  ( nodes, [] )


initialState : GameState
initialState =
  NotFinishedGame X emptyField [] 0


isEdgeOnField : Edge EdgeLabel -> Field -> Bool
isEdgeOnField edge field =
  let
    ( nodes, edges ) =
      field

    condition =
      \x -> ((x.first == edge.first && x.second == edge.second) || (x.first == edge.second && x.second == edge.first)) && edge.label.player == x.label.player
  in
    not <| isEmpty (List.filter condition edges)



{- Given a game state and a move this function returns if this move can be carried out -}


isValidMove : Move -> GameState -> Bool
isValidMove move state =
  let
    field =
      returnField state

    ( nodes, edges ) =
      field
  in
    case move of
      Entangled edge ->
        if (isCollapseNecessary field) then
          False
        else
          not <| isEdgeOnField edge field

      Collapse node ->
        if (isCollapseNecessary field) then
          let
            cycle =
              unsafe <| findCycle field

            ( cyclenodes, cycleedges ) =
              cycle

            potentialCollapsePoints =
              reachableFromComponent field cyclenodes

            edgesIncidentToNode =
              incidentEdges field node
          in
            if member node.id (List.map .id potentialCollapsePoints) && member (unsafe node.label.player) (List.map (\y -> y.label.player) edgesIncidentToNode) then
              True
            else
              False
        else
          False



{- check for a circle in the graph and then return if this collapse is possible -}


isCollapseNecessary : Field -> Bool
isCollapseNecessary field =
  isJust <| findCycle field



{- Adds a new move to the game -}


addMove : Move -> GameState -> GameState
addMove move state =
  let
    field =
      returnField state

    ( nodes, edges ) =
      field
  in
    if (isValidMove move state) then
      checkIfFinished
        <| case move of
            Entangled edge ->
              NotFinishedGame (switchPlayer <| unsafe <| returnPlayer state) ( nodes, { edge | label = { numberOfMove = returnRound state, player = unsafe <| returnPlayer state } } :: edges ) (move :: returnMoves state) ((returnRound state) + 1)

            Collapse node ->
              NotFinishedGame (unsafe <| returnPlayer state) ( updateNode node nodes, edges ) (move :: returnMoves state) (returnRound state)
    else
      state



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



--type alias NodeLabel = { pos: Position, numberOfMove: Int, player: Maybe Player }
--type alias EdgeLabel = { numberOfMove: Int, player: Player }
--type GameState = FinishedGame Result Field Moves | NotFinishedGame Player Field Moves Round


checkIfFinished : GameState -> GameState
checkIfFinished state =
  case state of
    FinishedGame result field moves ->
      FinishedGame result field moves

    NotFinishedGame player field moves round ->
      NotFinishedGame player field moves round



{- TODO -}
{- -}
--performEntireCollapse : Field -> Field


performEntireCollapse field =
  field



{- TODO -}
