module Counter where

import Effects exposing (Effects)
import Html
import Mouse

type alias Model = Int

type Action =
  Click

init: (Model, Effects Action)
init =
  (0 , Effects.none)

update: Action -> Model -> (Model, Effects Action)
update action model =
  (model + 1
  , Effects.none
  )

view: Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div [] [Html.text (toString model)]

inputs: List (Signal Action)
inputs =
  [Signal.map (\_ -> Click) Mouse.clicks]
