module App exposing (..)

import Html exposing (Html, div, text, form, input, textarea, button, select, option)
import Html.Attributes exposing (value, defaultValue, title, size, cols, rows, type')
import Html.App

-- MODEL

type Model
    = S String
    | I

init : (Model, Cmd Msg)
init =
    (I, Cmd.none)


-- MESSAGES

type Msg
    = NoOp


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        S string ->
            div []
                [ text string ]
        I ->
            div []
                [ text "Inputs"
                , form [title "title"] []
                , textarea [cols 30, rows 30, defaultValue "text"] []
                , button [value "value"] []
                , select [] []
                , option [] []
                ]


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- MAIN

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
