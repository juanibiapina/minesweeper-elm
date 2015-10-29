import StartApp
import Mouse
import Html
import Effects exposing (Effects)

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

app =
  StartApp.start { init = init , update = update, view = view, inputs = [Signal.map (\_ -> Click) Mouse.clicks] }

main: Signal Html.Html
main =
  app.html
