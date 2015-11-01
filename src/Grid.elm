module Grid where

import Array exposing (Array)
import Tile
import Effects exposing (Effects)
import Html
import Html.Attributes exposing (style)

type alias Model = Array (Array Tile.Model)

type Action = Open Int Int Tile.Action

gridWidth = 10
gridHeight = 10

neighborsOf: Int -> Int -> Model -> List Tile.Model
neighborsOf rowNumber columnNumber grid =
  let positions =
    [ (rowNumber - 1, columnNumber - 1)
    , (rowNumber - 1, columnNumber)
    , (rowNumber - 1, columnNumber + 1)
    , (rowNumber, columnNumber - 1)
    , (rowNumber, columnNumber + 1)
    , (rowNumber + 1, columnNumber - 1)
    , (rowNumber + 1, columnNumber)
    , (rowNumber + 1, columnNumber + 1)
    ]
  in
     List.filterMap (\(row, column) -> (Maybe.andThen (Array.get row grid) (Array.get column))) positions

emptyGrid: Model
emptyGrid =
  Array.repeat gridHeight (Array.repeat gridWidth Tile.empty)

fillMines: Model -> Model
fillMines grid =
  let pos = [(0, 0), (4, 3), (4, 4)]
  in
     List.foldl (\(rowNumber, columnNumber) grid ->
       let row = Array.get rowNumber grid
       in
          case row of
            Just row ->
              let tile = Array.get columnNumber row
              in
                 case tile of
                   Just tile -> Array.set rowNumber (Array.set columnNumber Tile.mine row) grid
                   Nothing -> grid
            Nothing -> grid
     ) grid pos

setValues: Model -> Model
setValues grid =
  let calculateValue rowNumber columnNumber tile =
    if Tile.isMine tile
    then
      tile
    else
      let neighbors = neighborsOf rowNumber columnNumber grid
      in
         Tile.value (List.foldl (\tile sum -> if Tile.isMine tile then sum + 1 else sum) 0 neighbors)
  in
     Array.indexedMap (\rowNumber row ->
       Array.indexedMap (\columnNumber tile ->
         calculateValue rowNumber columnNumber tile) row) grid

init: (Model, Effects Action)
init =
  (emptyGrid |> fillMines |> setValues, Effects.none)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Open rowNumber columnNumber tileAction ->
      (Array.indexedMap (\r row -> if r == rowNumber then Array.indexedMap (\c tile -> if c == columnNumber then Tile.open tile else tile) row else row ) model, Effects.none)

view: Signal.Address Action -> Model -> Html.Html
view address grid =
  let viewRow rowNumber row =
    Array.toList (Array.indexedMap (\columnNumber tile -> Tile.view (Signal.forwardTo address (Open rowNumber columnNumber)) tile) row)
  in
    Html.div [] (List.map (Html.div []) (Array.toList (Array.indexedMap viewRow grid)))
