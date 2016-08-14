import Html exposing (Html, div, h2, p)
import Html.App as App
import Element exposing (Element, toHtml)
import Keyboard exposing (KeyCode)
import Color exposing (..)
import Collage exposing (..)
import AnimationFrame 
import Time exposing (..)
import List exposing (..)
import Random

what : String
what = "A growing snake eating yellow fruits"

-- CONFIG
squareSizeInPixel : Int
squareSizeInPixel = 20

canvasSizeInSquare : Int
canvasSizeInSquare = 40


-- ENTRY POINT

main : Program Never
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions =
        (\_ -> Sub.batch
          [ Keyboard.downs (Keys << toKeyPress)
          , AnimationFrame.diffs Tick
          , Time.every (3 * second) CreateFruit
          ])
    }


-- MODEL

type alias Model =
  { snake : List (Int, Int)
  , fruits : List (Int, Int)
  , score : Int
  , direction : Direction
  , growing : Int
  , delta : Time
  }

init : (Model, Cmd Msg)
init =
  let initModel =
    { snake = createSnake 5
    , fruits = createFruits
    , score = 0
    , direction = Up
    , growing = 0
    , delta = 0
    }
  in
    (initModel, Cmd.none)

createSnake : Int -> List (Int, Int)
createSnake l =
  map (\n -> (0,n)) (reverse [0..(l-1)])


createFruits :  List (Int, Int)
createFruits =
  [(10,10), (15,30), (17,2)]


-- UPDATE

type Msg
  = Tick Time
  | Keys KeyPress
  | CreateFruit Time
  | NewFruit (Int,Int)
  | DoNothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Keys (Key direction) ->
      (model `turnToward` direction, Cmd.none)

    Tick t ->
      (moveForward model t, Cmd.none)

    CreateFruit t ->
      (model, Random.generate NewFruit fruitPositionGenerator)

    NewFruit (x,y) ->
      (newFruit model (x,y), Cmd.none)

    _ -> (model, Cmd.none)


turnToward : Model -> Direction -> Model
turnToward model direction =
  {model | direction = direction}


moveForward : Model -> Time -> Model
moveForward model t =
  let
    newDelta = model.delta + t
  in
    if newDelta > 180 then
      {model | delta = 0}
        |> forwardOneStep
        |> eatFruits
    else
      {model | delta = newDelta}


forwardOneStep : Model -> Model
forwardOneStep model =
  let
    (x,y) = snakeHead model.snake
    newHead =
      case model.direction of
        Left  -> (x-1, y)
        Right -> (x+1, y)
        Up    -> (x,   y+1)
        Down  -> (x,   y-1)

    newTail =
      if model.growing > 0 then
        model.snake
      else
        cutTail model.snake

    newGrowing =
      if model.growing > 0 then
        model.growing - 1
      else
        model.growing

  in
    {model | snake = newHead :: newTail, growing = newGrowing}


eatFruits : Model -> Model
eatFruits model =
  let
    head = snakeHead model.snake
  in
    if head `member` model.fruits then
      let
        newFruits = filter ((/=) head) model.fruits
      in
        { model
        | fruits = newFruits
        , score = model.score+100
        , growing = model.growing + 3
        }
    else
      model


newFruit : Model -> (Int,Int) -> Model
newFruit model (x,y) =
  {model | fruits = (x,y) :: model.fruits}


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ p [] [Html.text what]
    , viewScore model
    , toHtml (canvas model)
    ]

viewScore : Model -> Html Msg
viewScore model =
  p [] [ model.score |> toString |> Html.text ]



canvas : Model -> Element
canvas model =
  let
    canvasSize = squareSizeInPixel * canvasSizeInSquare
  in
    collage canvasSize canvasSize
      [ snakeForm model
      , fruitsForm model
      , coloredSquare (1,1) blue
      , coloredSquare (3,10) black
      , coloredSquare (39,39) green
      ]

snakeForm : Model -> Form
snakeForm model =
  coloredSquares model.snake red

fruitsForm : Model -> Form
fruitsForm model =
  coloredSquares model.fruits yellow

fruitPositionGenerator : Random.Generator ( Int, Int )
fruitPositionGenerator =
  Random.pair (Random.int 0 <| canvasSizeInSquare) (Random.int 0 <| canvasSizeInSquare-1)


-- INTERNALS

coloredSquare : (Int, Int) -> Color -> Form
coloredSquare (x,y) color =
  let
    squareSize = toFloat squareSizeInPixel
  in
    rect squareSize squareSize |> filled color |> atXY (x,y)

coloredSquares : List (Int, Int) -> Color -> Form
coloredSquares listOfPositions color =
  let
    squares = map (\ p -> coloredSquare p color ) listOfPositions
  in
    group squares

atXY : (Int, Int) -> Form -> Form
atXY (x, y) =
  let
    size = squareSizeInPixel
    halfSize = squareSizeInPixel // 2
    halfCanvas = canvasSizeInSquare // 2
    position l =
      (l - halfCanvas) * size + halfSize
  in
    move ( position x |> toFloat, position y |> toFloat)



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


snakeHead : List (Int,Int) -> (Int,Int)
snakeHead snake =
 case head snake of
   Just (x,y) -> (x,y)
   Nothing -> (0,0)

cutTail : List a -> List a
cutTail l =
  take (( length l ) - 1) l
