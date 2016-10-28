module Main exposing (..)
import  Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.App

type alias Model
    = Int

init : (Model, Cmd Msg)
init = (0, Cmd.none)

type Msg
    = Incr Int
    | Decr Int

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Incr 1) ] [ text "+ 1" ]
        , button [ onClick (Decr 1) ] [ text "- 1" ] 
        , text (toString model)
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Incr n ->
            (model + n, Cmd.none)
        Decr n ->
            ( model - n, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
