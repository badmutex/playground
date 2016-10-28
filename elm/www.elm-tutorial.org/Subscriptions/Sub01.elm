module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App
import Mouse
import Keyboard


type alias Model
    = Int

init : (Model, Cmd Msg)
init =
    ( 0, Cmd.none )

type Msg
    = MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode

view : Model -> Html Msg
view model =
    div []
        [ text (toString model) ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseMsg position ->
            ( model + 1, Cmd.none )

        KeyMsg code ->
            ( model + 2, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Keyboard.downs KeyMsg
        ]

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
