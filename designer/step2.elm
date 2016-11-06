import Html exposing (Html, div)
import Html.App as App
import Element exposing (Element, toHtml)
import Mouse
import Color exposing (..)
import Collage exposing (..)

import Debug

what = "Add box when mouse click"

squareSizeInPixel = 20
canvasSizeInSquare = 40

main =
  App.program
    { init = init 
    , update = update
    , view = view
    , subscriptions = 
        (\_ -> Sub.batch 
          [ Mouse.moves MouseMsg ]) 
    }

-- MODEL

type alias Model = (Int, Int)

init : (Model, Cmd Msg)
init = ((10,10), Cmd.none)


type Msg
  = DoNothing
  | MouseMsg Mouse.Position


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    MouseMsg position ->
        let
            _ = position |> toString  |> Debug.log "pos"
        in
            ((position.x, position.y), Cmd.none)

    _ -> 
        (model, Cmd.none)

   
-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ toHtml (canvas model)
    , Html.text what
    ]
   

canvas : Model -> Element
canvas (x, y) =
  let
    canvasSize = squareSizeInPixel * canvasSizeInSquare
    halfCanvas = canvasSize // 2
  in
    collage canvasSize canvasSize
      [ rect 10  10 |> filled red |> move (x - halfCanvas |> toFloat, halfCanvas - y |> toFloat)
      , coloredSquare blue (1,1)
      , coloredSquare black (3,10)
      , coloredSquare yellow (30,22)
      , coloredSquare green (39,39)
      ]


-- INTERNALS

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