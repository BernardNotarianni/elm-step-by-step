port module Chat exposing (..) -- where

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)

import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push

import Json.Encode as JE
import Json.Decode as JD exposing ((:=))

main : Program Never
main =
  App.program
    { init = (initialModel, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.Socket.listen model.phxSocket PhoenixMsg


-- MODEL

type alias Model =
  { messages : List String
  , typedMessage : String
  , phxSocket : Phoenix.Socket.Socket Msg
  }

initialModel : Model
initialModel =
  { messages = []
  , typedMessage = ""
  , phxSocket = initPhxSocket
  }

initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" "rooms:lobby" ReceiveChatMessage

-- UPDATE

type alias ChatMessage =
  { user : String
  , body : String
  }

type Msg
  = SendMessage
  | ReceiveChatMessage JE.Value
  | TypeMessage String
  | JoinChannel
  | PhoenixMsg (Phoenix.Socket.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TypeMessage message ->
      ( { model | typedMessage = message }, Cmd.none )

    SendMessage ->
      let
        -- We'll build our message out as a json encoded object
        payload = (JE.object [ ("user", JE.string "user"), ( "body", JE.string model.typedMessage ) ])

        -- We prepare to pushthe message
        push' = Phoenix.Push.init "new:msg" "rooms:lobby"
              |> Phoenix.Push.withPayload payload

        -- We update our `phxSocket` and `phxCmd` by passing this push
        -- into the Phoenix.Socket.push function
        ( phxSocket, phxCmd ) = Phoenix.Socket.push push' model.phxSocket
      in
        -- And we clear out the `newMessage` field, update our model's
        -- socket, and return our Phoenix command
        ( { model | typedMessage = "" , phxSocket = phxSocket} , Cmd.map PhoenixMsg phxCmd )

    ReceiveChatMessage raw ->
      case JD.decodeValue chatMessageDecoder raw of
        Ok chatMessage ->
          ( { model | messages = chatMessage.body :: model.messages }
          , Cmd.none
          )

        Err error -> ( model, Cmd.none )

    JoinChannel ->
      let
        channel = Phoenix.Channel.init "rooms:lobby"
        ( phxSocket, phxCmd ) = Phoenix.Socket.join channel model.phxSocket
      in
        ( { model | phxSocket = phxSocket } , Cmd.map PhoenixMsg phxCmd)
    PhoenixMsg msg ->
      let
        ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
      in
        ( { model | phxSocket = phxSocket } , Cmd.map PhoenixMsg phxCmd )

chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
  JD.object2 ChatMessage
      ("user" := JD.string)
      ("body" := JD.string)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ p [] [Html.text "hello chat"]
    , button [ onClick JoinChannel ] [ text "Join lobby" ]
    , form [ onSubmit SendMessage ]
           [ input [ placeholder "Message...", onInput TypeMessage, value model.typedMessage ] [] ]
    , viewMessages model.messages
    ]

viewMessages : List String -> Html Msg
viewMessages messages =
  let
    itemMessage m = li [] [ Html.text m ]
  in
    ul [] (List.map (\ m -> itemMessage m) messages)
