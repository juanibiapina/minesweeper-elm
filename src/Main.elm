import Counter
import StartApp
import Html

app =
  StartApp.start
    { init = Counter.init
    , update = Counter.update
    , view = Counter.view
    , inputs = Counter.inputs
    }

main: Signal Html.Html
main =
  app.html
