import Graphics.Element exposing (show, Element)
import Mouse

type alias Model = Int

initialModel: Int
initialModel =
  0

update: () -> Model -> Model
update _ model =
  model + 1

view: Model -> Element
view model =
  show model

main: Signal Element
main =
  Signal.map view (Signal.foldp update initialModel Mouse.clicks)
