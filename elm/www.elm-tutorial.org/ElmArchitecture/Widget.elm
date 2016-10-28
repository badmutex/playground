module Widget exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (title, name)
import Html.Events exposing (onClick)

type alias Model =
    { count : Int
    }

initialModel : Model
initialModel =
    { count = 0
    }

type Msg
    = Increase

view : Model -> Html Msg
view model =
    div [title "Widget Outer Div"]
        [ div [title "Widget Text Div"] [ text (toString model.count) ]
        , button [ title "Widget Button", onClick Increase ] [ text "Click" ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        Increase ->
            ( { model | count = model.count + 1 }, Cmd.none )
