import Html exposing (Html, div)
import Html.App as App
import Element exposing (Element, toHtml)
import Keyboard exposing (KeyCode)
import Color exposing (..)
import Collage exposing (..)
import AnimationFrame 
import Time exposing (..)

what = "A red box moving by itself, directed by keyboard"

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
          [ Keyboard.downs (Keys << toKeyPress)
          , AnimationFrame.diffs Tick
          ]) 
    }


-- MODEL

type alias Model = 
  { position : (Int, Int)
  , direction : Direction
  , delta : Time
  }

init : (Model, Cmd Msg)
init = 
  let initModel =
    { position = (0,0)
    , direction = Right
    , delta = 0
    }
  in 
    (initModel, Cmd.none)



-- UPDATE

type Msg
  = Tick Time 
  | Keys KeyPress
  | DoNothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Keys (Key direction) ->
      (model `turnToward` direction, Cmd.none)

    Tick t ->
      (moveForward model t, Cmd.none)
      

    _ -> (model, Cmd.none)


turnToward : Model -> Direction -> Model
turnToward model direction =
  let
    (x,y) = model.position
    newPosition =
      case direction of
        Left  -> (x-1, y)
        Right -> (x+1, y)
        Up    -> (x,   y+1)
        Down  -> (x,   y-1)
  in
    {model | direction = direction}


moveForward : Model -> Time -> Model
moveForward model t =
  let
    newDelta = model.delta + t
  in
    if newDelta > 180 then
      forwardOneStep {model | delta = 0}
    else  
      {model | delta = newDelta}
 

forwardOneStep : Model -> Model
forwardOneStep model =
  let
    (x,y) = model.position
    newPosition =
      case model.direction of
        Left  -> (x-1, y)
        Right -> (x+1, y)
        Up    -> (x,   y+1)
        Down  -> (x,   y-1)
  in
    {model | position = newPosition}

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
      [ coloredSquare red model.position
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