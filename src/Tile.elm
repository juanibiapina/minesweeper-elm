module Tile where

import Effects exposing (Effects)
import Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Mouse

type State = Opened | Closed
type Content = Mine | Value Int

type alias Tile =
  { state: State
  , content: Content
  }

type Action = Open

tileSize = "50px"

empty: Tile
empty =
  { state = Closed
  , content = Value 0
  }

value: Int -> Tile
value v =
  { state = Closed
  , content = Value v
  }

mine: Tile
mine =
  { state = Closed
  , content = Mine
  }

isMine: Tile -> Bool
isMine tile =
  case tile.content of
    Mine -> True
    _ -> False

open: Tile -> Tile
open tile =
  { tile | state <- Opened }

view: Signal.Address Action -> Tile -> Html.Html
view address tile =
  case tile.state of
    Opened ->
      case tile.content of
        Mine -> Html.button [mineStyle] []
        Value v -> Html.button [openedStyle] [Html.text (toString v)]
    Closed -> Html.button [onClick address Open, closedStyle] []

mineStyle: Html.Attribute
mineStyle =
  style
    [ ("width", tileSize)
    , ("height", tileSize)
    , ("background-color", "red")
    , ("vertical-align", "top")
    ]

openedStyle: Html.Attribute
openedStyle =
  style
    [ ("width", tileSize)
    , ("height", tileSize)
    , ("background-color", "white")
    , ("vertical-align", "top")
    ]

closedStyle: Html.Attribute
closedStyle =
  style
    [ ("width", tileSize)
    , ("height", tileSize)
    , ("background-color", "blue")
    , ("vertical-align", "top")
    ]
