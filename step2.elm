import Html exposing (div)
import Element exposing (Element, toHtml)

import Color exposing (..)
import Collage exposing (..)

what = "Display a blue box and cutted red box"

main =
    div [] 
    [ Html.text what
    , view
    ]
    
view =
    toHtml canvas

canvas =
  let
    canvasSize = 500
  in
    collage canvasSize canvasSize
      [ rect 10 10 |> filled red |> moveXY (250,250)
      , rect 10 10 |> filled blue |> moveXY (20,50)
      ]
      
moveXY : (Int, Int) -> Form -> Form
moveXY (x, y) =
  move ( x |> toFloat , y |> toFloat )