module GameLogic (..) where

import Debug exposing (crash)
import Graph exposing (..)
import Helpers exposing (..)
import List exposing (..)
import Maybe exposing (..)

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
            componentWithCycle =
              unsafe <| findComponentWithCycle field

            edgesIncidentToNode =
              incidentEdges field node
          in
            if member node.id (List.map .id componentWithCycle) && member (unsafe node.label.player) (List.map (\y -> y.label.player) edgesIncidentToNode) then
              True
            else
              False
        else
          False



{- check for a circle in the graph and then return if this collapse is possible -}


isCollapseNecessary : Field -> Bool
isCollapseNecessary field =
  isJust <| findComponentWithCycle field



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
                  NotFinishedGame (unsafe <| returnPlayer state) (performEntireCollapse <| replaceNode field node (unsafe <| node.label.player)) (move :: returnMoves state) (returnRound state)
    else
      state

replaceNode : Field -> Node NodeLabel -> Player -> Field
replaceNode field node p = let
                  (nodes, edges) = field
                  newNumberOfMove = unsafe <| maximum <| List.map (\x -> x.label.numberOfMove) <| List.filter (\x -> x.label.player == unsafe node.label.player) (incidentEdges field node)
                  l = node.label
                  newLabel = {l | numberOfMove = newNumberOfMove, player = Just p}
                  newNode = {node | label = newLabel}
  in
    ( updateNode newNode nodes, edges )


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
performEntireCollapse : Field -> Field
performEntireCollapse field = let
    (nodes,edges) = field
  in
    field {-TODO-}

collapseOneEdge : Field -> Edge EdgeLabel -> Field
collapseOneEdge field edge = let
    (nodes, edges) = field
    firstNode = unsafe <| getNodeFromId field edge.first
    secondNode = unsafe <| getNodeFromId field edge.second
    edgePlayer = edge.label.player
  in
    case firstNode.label.player of
        Nothing -> case secondNode.label.player of
                        Nothing -> field
                        Just player -> if player == edgePlayer then removeEdge field edge else removeEdge (replaceNode field firstNode edgePlayer) edge
        Just player -> if player == edgePlayer then removeEdge field edge else removeEdge (replaceNode field secondNode edgePlayer) edge  


{-
type alias NodeLabel =
  { pos : Position, numberOfMove : Int, player : Maybe Player }


type alias EdgeLabel =
  { numberOfMove : Int, player : Player }

-}
