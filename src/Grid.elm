module Grid where

import Array exposing (Array)
import Tile
import Effects exposing (Effects)
import Html

type alias Model = Array Tile.Model

type Action = Open Int Tile.Action

neighborsOf: Int -> Model -> List Tile.Model
neighborsOf index grid =
  let pos = [index - 1, index + 1]
  in
     List.filterMap (\index -> Array.get index grid) pos

emptyGrid: Model
emptyGrid =
  Array.fromList([Tile.empty, Tile.empty, Tile.empty])

fillMines: Model -> Model
fillMines grid =
  let pos = [0, 2]
  in
     Array.indexedMap (\index tile -> if List.any (\pos -> index == pos) pos then Tile.mine else tile) grid

setValues: Model -> Model
setValues grid =
  let calculateValue index tile =
    if Tile.isMine tile
    then
      tile
    else
      let neighbors = neighborsOf index grid
      in
         Tile.value (List.foldl (\tile sum -> if Tile.isMine tile then sum + 1 else sum) 0 neighbors)
  in
     Array.indexedMap calculateValue grid

init: (Model, Effects Action)
init =
  (emptyGrid |> fillMines |> setValues, Effects.none)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Open index tileAction -> (Array.indexedMap (\i tile -> if i == index then Tile.open tile else tile ) model, Effects.none)

view: Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div [] (Array.toList (Array.indexedMap (\index tile -> Tile.view (Signal.forwardTo address (Open index)) tile) model))
