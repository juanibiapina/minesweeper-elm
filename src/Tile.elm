module Tile where

import Effects exposing (Effects)
import Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Mouse

type State = Opened | Closed

type alias Model =
  { state: State
  }

type Action = Open

init: (Model, Effects Action)
init =
  ({ state = Closed } , Effects.none)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Open -> ({ model | state <- Opened }, Effects.none)

view: Signal.Address Action -> Model -> Html.Html
view address model =
  case model.state of
    Opened -> Html.button [openedStyle] []
    Closed -> Html.button [onClick address Open, closedStyle] []

inputs: List (Signal Action)
inputs =
  []

openedStyle: Html.Attribute
openedStyle =
  style
    [ ("width", "100px")
    , ("height", "100px")
    , ("background-color", "white")
    ]

closedStyle: Html.Attribute
closedStyle =
  style
    [ ("width", "100px")
    , ("height", "100px")
    , ("background-color", "blue")
    ]
