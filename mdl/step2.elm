import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (href, class, style)

import Material
import Material.Scheme
import Material.Button as Button
import Material.Options exposing (css)
import Material.Layout as Layout
import Material.Color as Color

main : Program Never
main =
  App.program 
    { init = ( model, Cmd.none ) 
    , view = view
    , subscriptions = always Sub.none 
    , update = update
    }



-- MODEL

type alias Model = 
  { count : Int
  , selectedTab : Int
  , mdl : Material.Model 
  }


model : Model 
model = 
  { count = 0
   , selectedTab = 0
  , mdl = Material.model
  }


-- ACTION, UPDATE

type Msg
  = Increase
  | Reset
  | SelectTab Int
  | Mdl (Material.Msg Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increase -> 
      ( { model | count = model.count + 1 } 
      , Cmd.none
      )

    Reset -> 
      ( { model | count = 0 }
      , Cmd.none
      )

    SelectTab num ->
      { model | selectedTab = num } ! []

    Mdl msg' -> 
      Material.update msg' model


-- VIEW


type alias Mdl = 
  Material.Model 

view : Model -> Html Msg
view model =
  Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
    Layout.render Mdl
      model.mdl
      [ Layout.fixedHeader
      , Layout.selectedTab model.selectedTab
      , Layout.onSelectTab SelectTab
      ]
      { header = [ h1 [ style [ ( "padding", "2rem" ) ] ] [ text "Counter with a MDL layout" ] ]
      , drawer = []
      , tabs = ( [ text "Counter", text "Other" ], [ Color.background (Color.color Color.Teal Color.S400) ] )
      , main = [ viewSelectedTab model ]
      }


viewSelectedTab : Model -> Html Msg
viewSelectedTab model =
  case model.selectedTab of
    0 ->
        viewCounter model

    1 ->
        text "something else"

    _ ->
        text "404"

viewCounter  : Model -> Html Msg
viewCounter  model =
  div 
    [ style [ ("padding", "2rem") ] ]
    [ text ("Current count: " ++ toString model.count )
   
    , Button.render Mdl [0] model.mdl 
        [ Button.onClick Increase ]
        [ text "Increase" ]
    , Button.render Mdl [1] model.mdl 
        [ Button.onClick Reset ] 
        [ text "Reset" ]
    ]
  

