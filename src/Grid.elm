module Grid where

import Array exposing (Array)
import Tile
import Effects exposing (Effects)
import Html
import Html.Attributes exposing (style)
import Random
import Maybe exposing (andThen)

type alias Location = (Int, Int)
type alias Grid = Array (Array Tile.Tile)

type Action = Open Location Tile.Action

numberOfColumns = 10
numberOfRows = 10
numberOfMines = 20

getTile: Location -> Grid -> Maybe Tile.Tile
getTile (rowNumber, columnNumber) grid =
  (Array.get rowNumber grid) `andThen` (Array.get columnNumber)

neighborLocations: Location -> List Location
neighborLocations (rowNumber, columnNumber) =
  [ (rowNumber - 1, columnNumber - 1)
  , (rowNumber - 1, columnNumber)
  , (rowNumber - 1, columnNumber + 1)
  , (rowNumber, columnNumber - 1)
  , (rowNumber, columnNumber + 1)
  , (rowNumber + 1, columnNumber - 1)
  , (rowNumber + 1, columnNumber)
  , (rowNumber + 1, columnNumber + 1)
  ]

neighborsOf: Location -> Grid -> List Tile.Tile
neighborsOf location grid =
  List.filterMap (flip getTile grid) (neighborLocations location)

emptyGrid: Grid
emptyGrid =
  Array.repeat numberOfRows (Array.repeat numberOfColumns Tile.empty)

generateMinePositions: Int -> List Location
generateMinePositions seed =
  let (result, seed') = Random.generate (Random.list numberOfMines (Random.pair (Random.int 0 (numberOfRows - 1)) (Random.int 0 (numberOfColumns - 1)))) (Random.initialSeed seed)
  in
     result

placeMines: List Location -> Grid -> Grid
placeMines positions grid =
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
   ) grid positions

fillMines: Grid -> Grid
fillMines grid =
  placeMines (generateMinePositions 31416) grid

calculateValues: Grid -> Grid
calculateValues grid =
  let calculateValue location tile =
    if Tile.isMine tile
    then
      tile
    else
      let neighbors = neighborsOf location grid
      in
         Tile.value (List.foldl (\tile sum -> if Tile.isMine tile then sum + 1 else sum) 0 neighbors)
  in
     Array.indexedMap (\rowNumber row ->
       Array.indexedMap (\columnNumber tile ->
         calculateValue (rowNumber, columnNumber) tile) row) grid

init: (Grid, Effects Action)
init =
  (emptyGrid |> fillMines |> calculateValues, Effects.none)

floodOpen: Location -> Grid -> Grid
floodOpen location grid =
  let tile = getTile location grid
  in
     case tile of
       Just tile ->
         if Tile.isZero tile
         then
           List.foldl openTile grid (neighborLocations location)
         else grid
       Nothing -> grid

openTile: Location -> Grid -> Grid
openTile (rowNumber, columnNumber) grid =
  let tile = getTile (rowNumber, columnNumber) grid
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
         then floodOpen (rowNumber, columnNumber) gridWithOpenTile
         else grid
       Nothing -> grid


update: Action -> Grid -> (Grid, Effects Action)
update action grid =
  case action of
    Open location tileAction ->
      (openTile location grid, Effects.none)

view: Signal.Address Action -> Grid -> Html.Html
view address grid =
  let viewRow rowNumber row =
    Array.toList (Array.indexedMap (\columnNumber tile -> Tile.view (Signal.forwardTo address (Open (rowNumber, columnNumber))) tile) row)
  in
     Html.div [] (List.map (Html.div []) (Array.toList (Array.indexedMap viewRow grid)))
