import Graphics.Element exposing (show, Element)

type alias Model = String

model: Model
model =
  "hello world"

view: Model -> Element
view model =
  show model

main: Element
main =
  view model
