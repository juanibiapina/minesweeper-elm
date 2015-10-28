import Graphics.Element exposing (show, Element)

type alias Model = Int

model: Int
model =
  0

view: Model -> Element
view model =
  show model

main: Element
main =
  view model
