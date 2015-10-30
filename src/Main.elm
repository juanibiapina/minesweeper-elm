import Grid
import StartApp
import Html

app =
  StartApp.start
    { init = Grid.init
    , update = Grid.update
    , view = Grid.view
    , inputs = []
    }

main: Signal Html.Html
main =
  app.html
