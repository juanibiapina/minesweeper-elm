module Grid where

import Tile
import Effects exposing (Effects)
import Html

type alias Model = List Tile.Model

type Action = Open Int Tile.Action

init: (Model, Effects Action)
init =
  ([Tile.makeMine, Tile.makeMine], Effects.none)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Open index tileAction -> (List.indexedMap (\i tile -> if i == index then Tile.open tile else tile ) model, Effects.none)

view: Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div [] (List.indexedMap (\index tile -> Tile.view (Signal.forwardTo address (Open index)) tile) model)
