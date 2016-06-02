import Html exposing (Html, div)
import Html.App as App
import Element exposing (Element, toHtml)
import Keyboard exposing (KeyCode)
import Color exposing (..)
import Collage exposing (..)

what = "A red box moved by keyboard"

-- CONFIG

squareSizeInPixel = 20
canvasSizeInSquare = 40


-- ENTRY POINT

main =
  App.program
    { init = init 
    , update = update
    , view = view
    , subscriptions = 
        (\_ -> Sub.batch 
          [ Keyboard.downs (Keys << toKeyPress)]) 
    }


-- MODEL

type alias Model = (Int, Int)

init : (Model, Cmd Msg)
init = ((10,10), Cmd.none)


type Msg
  = DoNothing
  | Keys KeyPress


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Keys (Key direction) ->
      (model `moveToward` direction, Cmd.none)

    _ -> (model, Cmd.none)


moveToward : Model -> Direction -> Model
moveToward (x,y) direction =
  case direction of
    Left  -> (x-1, y)
    Right -> (x+1, y)
    Up    -> (x,   y+1)
    Down  -> (x,   y-1)

    
-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ Html.text what
    , toHtml (canvas model)
    ]
    

canvas : Model -> Element
canvas model =
  let
    canvasSize = squareSizeInPixel * canvasSizeInSquare
  in
    collage canvasSize canvasSize
      [ coloredSquare red model
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



type Direction = Left | Right | Up | Down 

type KeyPress = Key Direction | OtherKeys

toKeyPress : KeyCode -> KeyPress
toKeyPress keyCode =
  case keyCode of 
    38 -> Key Up
    40 -> Key Down
    37 -> Key Left
    39 -> Key Right
    _ -> OtherKeys