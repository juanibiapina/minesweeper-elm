module Grid where

import Array exposing (Array)
import Tile
import Effects exposing (Effects)
import Html
import Html.Attributes exposing (style)

type alias Grid = Array (Array Tile.Tile)

type Action = Open Int Int Tile.Action

gridWidth = 10
gridHeight = 10

neighborsOf: Int -> Int -> Grid -> List Tile.Tile
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

emptyGrid: Grid
emptyGrid =
  Array.repeat gridHeight (Array.repeat gridWidth Tile.empty)

fillMines: Grid -> Grid
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

setValues: Grid -> Grid
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

init: (Grid, Effects Action)
init =
  (emptyGrid |> fillMines |> setValues, Effects.none)

openTile: Int -> Int -> Grid -> Grid
openTile rowNumber columnNumber grid =
  let openTileInColumn c tile =
        if c == columnNumber
          then Tile.open tile
          else tile
      openTileInRow r row =
        if r == rowNumber
          then Array.indexedMap openTileInColumn row
          else row
  in
     Array.indexedMap openTileInRow grid


update: Action -> Grid -> (Grid, Effects Action)
update action grid =
  case action of
    Open rowNumber columnNumber tileAction ->
      (openTile rowNumber columnNumber grid, Effects.none)

view: Signal.Address Action -> Grid -> Html.Html
view address grid =
  let viewRow rowNumber row =
    Array.toList (Array.indexedMap (\columnNumber tile -> Tile.view (Signal.forwardTo address (Open rowNumber columnNumber)) tile) row)
  in
     Html.div [] (List.map (Html.div []) (Array.toList (Array.indexedMap viewRow grid)))
