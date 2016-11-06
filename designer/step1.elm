import Html exposing (div)
import Element exposing (Element, toHtml)

import Color exposing (..)
import Collage exposing (..)

what = "Several colored boxes"

squareSizeInPixel = 20
canvasSizeInSquare = 40

main =
    div [] 
    [ Html.text what
    , view
    ]
    
view =
    toHtml canvas

canvas =
  let
    canvasSize = squareSizeInPixel * canvasSizeInSquare
  in
    collage canvasSize canvasSize
      [ coloredSquare red (0,0)
      , coloredSquare blue (1,1)
      , coloredSquare black (3,10)
      , coloredSquare yellow (30,22)
      , coloredSquare green (39,39)
      ]
      
coloredSquare color (x,y)  =   
  rect squareSizeInPixel squareSizeInPixel |> filled color |> atXY (x,y)

atXY : (Int, Int) -> Form -> Form
atXY (x, y) =
  let
    size = squareSizeInPixel
    halfSize = squareSizeInPixel // 2
    halfCanvas = canvasSizeInSquare // 2
    position l = 
      (l-halfCanvas) * size + halfSize
  in 
    move ( position x |> toFloat , position y |> toFloat )