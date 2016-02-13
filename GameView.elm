module GameView where

import GameLogic exposing (..)

import Color exposing (black)
import Text exposing (fromString, bold)
import List exposing (..)
--import List.Split
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (button)
import Signal exposing (..)
import Mouse

type alias Input = (Int,Int)

{-The length of one side of the game board-}
size = 600

{-Double lines to show that we are playing quantumtictactoe and not just regular tictactoe
-}
drawLines : Element
drawLines = collage size size [
        outlined (solid black) (rect 3 size) |> move (-(size/6),0),
        outlined (solid black) (rect 3 size) |> move ((size/6),0),
        outlined (solid black) (rect size 3) |> move (0,-(size/6)),
        outlined (solid black) (rect size 3) |> move (0,(size/6))]

{-The basic forms for the two players-}
xline = filled Color.blue (rect 8 (1/4*size))
xcross = group [rotate (degrees 45) xline, rotate (degrees -45) xline]
ocircle = outlined ({defaultLine | width = 8, color = Color.red}) (circle (size/10))

printClassicalMarks : Field -> List Form
printClassicalMarks field =
  let
    (nodes, edges) = field
  in
    (List.map (group << printClassicalNode << .label) nodes)

extractOneNode : Field -> Node NodeLabel -> (NodeLabel, List EdgeLabel)
extractOneNode field node = 
  let
    (nodes, edges) = field
    {id, label} = node
  in
    (label, List.map .label (incidentEdges field node))

printEntangeledMarks : Field -> List Form
printEntangeledMarks field = 
  let
    (nodes, edges) = field
  in
    List.map (formatOneField << (uncurry listEntangledNode) << (extractOneNode field)) nodes

printClassicalNode : NodeLabel -> List Form
printClassicalNode { pos, numberOfMove, player } = 
  let
    {row, col} = pos
    moveAll = move (-(size/3) + (toFloat col - 1) * (size/3), size/3 - (toFloat row - 1) * (size/3))
    moveText = move (size/9,-size/9)
  in
    case player of
        Nothing -> []
        Just X -> [moveAll <| group [ xcross, moveText <| scale 3 <|toForm <| show numberOfMove]]
        Just O -> [moveAll <| group [ ocircle, moveText <| scale 3 <| toForm <| show numberOfMove]]
{--
chunksOfLeft : Int -> List a -> List (List a)
chunksOfLeft k xs =
  let len = length xs
  in  if len > k
      then take k xs :: chunksOfLeft k (drop k xs)
      else [xs]
-}

{-Takes a list of at most nine forms and then aligns them properly
to fit in one position on the game field-}
formatOneField : List Form -> Form
formatOneField forms =
  let
    moveCoordinates = tuples [-size/9, 0 , size/9] [size/9, 0 , -size/9]
  in
    group <| List.map2 (<|) (List.map move moveCoordinates) forms

{-Gives a list of forms of all the entangeled marks in this game field position,
already moved to be in the right position,
but this still has to be formatted inside of the position before printing-}
listEntangledNode : NodeLabel -> List EdgeLabel -> List Form
listEntangledNode {pos} edgelabels =
  let
    {col, row} = pos
    moveToRightPosition = move (-(size/3) + (toFloat col - 1) * (size/3), size/3 - (toFloat row - 1) * (size/3))
  in 
    List.map (moveToRightPosition << printEntangeledEdge) edgelabels

{-Returns a small version of a mark to represent that they are entangeled-}
printEntangeledEdge : EdgeLabel -> Form
printEntangeledEdge {numberOfMove, player} = case player of
        X -> scale (1/3) <| group [xcross, move (size/9,-size/9) <| scale 3 <| toForm <| show numberOfMove]
        O -> scale (1/3) <| group [ocircle, move (size/9,-size/9) <| scale 3 <| toForm <| show numberOfMove]

{-Status message to show to the player-}
--type GameState = FinishedGame Result Field Moves | NotFinishedGame Player Field Moves
--type Result = Draw | Tie Player | Winner Player
stateDescription : GameState -> String
stateDescription state = case state of
        FinishedGame result _ _ -> case result of
                Draw -> "Game over. Since nobody has three classical marks in the right position it is a Draw"
                Tie p -> "Game over. Both players got three classical marks in a line, so we have a tie. 1 Point: " ++ toString p ++ "1/2 Points: " ++ (toString <| switchPlayer p)
                Winner p -> "Game over. Winner: " ++ toString p
        NotFinishedGame p _ _ _ -> "Next move: " ++ toString p

newGameMailbox : Mailbox ()
newGameMailbox = mailbox ()

newGameButton : Element
newGameButton = button (message newGameMailbox.address ()) "New Game"

view : GameState -> Element
view state =
  let
    field = returnField state
    collapseMessage = if isCollapseNecessary field then "Collapse necessary" else "Please insert entangeled marks"
  in
    flow down [
          layers [drawLines, collage size size (printClassicalMarks field), collage size size (printEntangeledMarks field)],
          container size 40 middle <| leftAligned <| fromString <| stateDescription state,
          container size 40 middle <| leftAligned <| fromString collapseMessage,
          container size 50 middle <| flow right [newGameButton],
          collage 1500 120 [toForm <| show (reachableFromNode field testNode3)]
    ]

clickSignal : Signal (Int,Int)
clickSignal = sampleOn Mouse.clicks Mouse.position

newGameButtonSignal : Signal ()
newGameButtonSignal = newGameMailbox.signal

{-There are two possible types of Moves: Enter entangeled marks or collapse a game situation
- The Maybe (Int,Int) is there to hold the position of the first click
-}
update: Input -> (GameState,Maybe (Int,Int)) -> (GameState,Maybe (Int,Int))
update (x,y) (state,firstClick) = let
    nodeLabel = {pos = getPosition (x,y), numberOfMove = returnRound state, player = returnPlayer state}
  in
    case firstClick of
        Nothing -> case (isCollapseNecessary <| returnField state) of
                        True -> (addMove (Collapse {id = (idFromPosition <| getPosition (x,y)), label = nodeLabel}) state, Nothing)
                        False ->  (state, Just (x,y))
        Just (xold,yold) -> (addMove (Entangled {first = idFromPosition <| getPosition (x,y), second = idFromPosition <| getPosition (xold,yold), label = {numberOfMove = returnRound state, player = unsafe <| returnPlayer state}}) state, Nothing)
{-
type alias Node a =
        { id : NodeId
        , label : a
        }
type alias NodeLabel = { pos: Position, numberOfMove: Int, player: Maybe Player }

type alias Edge e =
        { first : NodeId
        , second : NodeId
        , label : e
        }
type alias EdgeLabel = { numberOfMove: Int, player: Player }   
-}

--type alias Position = { col: Int, row: Int }
getPosition : (Int, Int) -> GameLogic.Position
getPosition (x,y) = let
    f a = if (a < (size // 3)) then 1 else if (a < 2*size // 3) then 2 else 3
  in
    {col = f x, row = f y}

idFromPosition : GameLogic.Position -> Int
idFromPosition {col, row} = 3*(col-1) + row

gameStateSignal : Signal (GameState,Maybe (Int,Int))
gameStateSignal = Signal.foldp update (initialState,Nothing) clickSignal 

main : Signal Element
main = Signal.map view (Signal.map fst gameStateSignal)



{-Some definitions for testing the functions-}
testNode2 = { id = 4, label = { pos = { col = 2, row = 1 }, numberOfMove = -1, player = Nothing } }
testNode3 = { id = 6, label = { pos = { col = 2, row = 3 }, numberOfMove = -1, player = Nothing } }

testField = ([{ id = 1, label = { pos = { col = 1, row = 1 }, numberOfMove = 3, player = Just X } },{ id = 2, label = { pos = { col = 1, row = 2 }, numberOfMove = -1, player = Nothing } },{ id = 3, label = { pos = { col = 1, row = 3 }, numberOfMove = 5, player = Just O } },{ id = 4, label = { pos = { col = 2, row = 1 }, numberOfMove = -1, player = Nothing } },{ id = 5, label = { pos = { col = 2, row = 2 }, numberOfMove = -1, player = Nothing } },{ id = 6, label = { pos = { col = 2, row = 3 }, numberOfMove = -1, player = Nothing } },{ id = 7, label = { pos = { col = 3, row = 1 }, numberOfMove = -1, player = Nothing } },{ id = 8, label = { pos = { col = 3, row = 2 }, numberOfMove = -1, player = Nothing } },{ id = 9, label = { pos = { col = 3, row = 3 }, numberOfMove = -1, player = Nothing } }],[{ first = 4, second = 8, label = {numberOfMove = 7, player = X}}, { first = 8, second = 6, label = {numberOfMove = 8, player = O}}, { first = 9, second = 6, label = {numberOfMove = 3, player = O}}, { first = 8, second = 6, label = {numberOfMove = 4, player = X}}, { first = 9, second = 2, label = {numberOfMove = 12, player = X}}, { first = 2, second = 6, label = {numberOfMove = 6, player = O}}])

testState = NotFinishedGame X testField []
testState2 = FinishedGame (Tie O) testField []
