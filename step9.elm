import Html exposing (Html, div, h2, p)
import Html.Attributes exposing (style)
import Html.App as App
import Element exposing (Element, toHtml)
import Keyboard exposing (KeyCode)
import Color exposing (..)
import Collage exposing (..)
import Text exposing (fromString, height)
import AnimationFrame 
import Time exposing (..)
import List exposing (..)
import Random

what : String
what = "A growing snake dying when he eats his own tail"

-- CONFIG
squareSizeInPixel : Int
squareSizeInPixel = 20

canvasSizeInSquare : Int
canvasSizeInSquare = 20


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
  { running : Bool
  , snake : List (Int, Int)
  , fruits : List (Int, Int)
  , score : Int
  , direction : Direction
  , growing : Int
  , delta : Time
  }

init : (Model, Cmd Msg)
init =
  let initModel =
    { running = True
    , snake = createSnake 5
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
  if not model.running then
    (model, Cmd.none)
  else
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
  case (model.direction, direction) of
    (Up, Down) -> model
    (Down, Up) -> model
    (Left, Right) -> model
    (Right, Left) -> model
    _ -> {model | direction = direction}


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
    newHead =
      snakeHead model.snake
        |> moveOneStep model.direction
        |> warpWhenBeyondEdge

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
    if newHead `member` model.snake then
      { model | running = False }
    else
      { model | snake = newHead :: newTail, growing = newGrowing }


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
    gameForms =
      [ background
      , snakeForm model
      , fruitsForm model
      ]
    forms =
      if model.running then
        gameForms
      else
        reverse (gameOver :: gameForms)
  in
    collage canvasSize canvasSize forms

snakeForm : Model -> Form
snakeForm model =
  coloredSquares model.snake red

fruitsForm : Model -> Form
fruitsForm model =
  coloredSquares model.fruits yellow

background : Form
background =
  let
    squareSize = (squareSizeInPixel * canvasSizeInSquare) |> toFloat
  in
    rect squareSize squareSize |> filled grey

gameOver : Form
gameOver =
  "Game Over"
    |> fromString
    |> height 40
    |> text

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

fruitPositionGenerator : Random.Generator ( Int, Int )
fruitPositionGenerator =
  Random.pair (Random.int 0 <| canvasSizeInSquare-1) (Random.int 0 <| canvasSizeInSquare-1)

moveOneStep : Direction -> (Int,Int) -> (Int,Int)
moveOneStep direction (x,y) =
  case direction of
    Left  -> (x-1, y)
    Right -> (x+1, y)
    Up    -> (x,   y+1)
    Down  -> (x,   y-1)

warpWhenBeyondEdge : (Int,Int) -> (Int,Int)
warpWhenBeyondEdge (x,y) =
  (warp x, warp y)

warp : Int -> Int
warp a =
  if a < 0 then canvasSizeInSquare - 1
  else if a >= canvasSizeInSquare then 0
  else a
