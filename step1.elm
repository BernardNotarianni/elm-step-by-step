import Html exposing (text, div)
import Color exposing (Color, lightBlue, purple, blue, black, white)
import Element exposing (Element, toHtml, color, container, topLeftAt, centered, absolute)
import Text exposing (Text)

what = "Display a blue box containing text 'hello'"

main =
    div [] 
    [ text what
    , view
    ]
    
view =
  let
    (width, height) = (500, 500)
    position = topLeftAt (absolute 50) (absolute 20)
  in
    toHtml
      <| color blue
      <| container width height position
      <| centered
      <| coloredText "hello" white


coloredText : String -> Color -> Text
coloredText text color = (Text.color color (Text.fromString text))