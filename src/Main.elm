import Tile
import StartApp
import Html

app =
  StartApp.start
    { init = Tile.init
    , update = Tile.update
    , view = Tile.view
    , inputs = Tile.inputs
    }

main: Signal Html.Html
main =
  app.html
