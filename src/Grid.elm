module Grid where

import Array exposing (Array)
import Tile
import Effects exposing (Effects)
import Html
import Html.Attributes exposing (style)

type alias Grid = Array (Array Tile.Tile)

type Action = Open Int Int Tile.Action

numberOfColumns = 10
numberOfRows = 10
numberOfMines = 10

neighborPositions: Int -> Int -> List (Int, Int)
neighborPositions rowNumber columnNumber =
  [ (rowNumber - 1, columnNumber - 1)
  , (rowNumber - 1, columnNumber)
  , (rowNumber - 1, columnNumber + 1)
  , (rowNumber, columnNumber - 1)
  , (rowNumber, columnNumber + 1)
  , (rowNumber + 1, columnNumber - 1)
  , (rowNumber + 1, columnNumber)
  , (rowNumber + 1, columnNumber + 1)
  ]

neighborsOf: Int -> Int -> Grid -> List Tile.Tile
neighborsOf rowNumber columnNumber grid =
  List.filterMap (\(row, column) -> (Maybe.andThen (Array.get row grid) (Array.get column))) (neighborPositions rowNumber columnNumber)

emptyGrid: Grid
emptyGrid =
  Array.repeat numberOfRows (Array.repeat numberOfColumns Tile.empty)

fillMines: Grid -> Grid
fillMines grid =
  let minePositions = [(0, 0), (4, 3), (4, 4)]
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
     ) grid minePositions

calculateValues: Grid -> Grid
calculateValues grid =
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
  (emptyGrid |> fillMines |> calculateValues, Effects.none)

floodOpen: Int -> Int -> Grid -> Grid
floodOpen rowNumber columnNumber grid =
  let tile = Maybe.andThen (Array.get rowNumber grid) (Array.get columnNumber)
  in
     case tile of
       Just tile ->
         if Tile.isZero tile
         then
           List.foldl (\(row, column) grid -> openTile row column grid) grid (neighborPositions rowNumber columnNumber)
         else grid
       Nothing -> grid

openTile: Int -> Int -> Grid -> Grid
openTile rowNumber columnNumber grid =
  let tile = Maybe.andThen (Array.get rowNumber grid) (Array.get columnNumber)
      openTileInColumn c tile =
        if c == columnNumber
          then Tile.open tile
          else tile
      openTileInRow r row =
        if r == rowNumber
          then Array.indexedMap openTileInColumn row
          else row
      gridWithOpenTile = Array.indexedMap openTileInRow grid
  in
     case tile of
       Just tile ->
         if Tile.isClosed tile
         then floodOpen rowNumber columnNumber gridWithOpenTile
         else grid
       Nothing -> grid


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
