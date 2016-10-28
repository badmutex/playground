module App exposing (..)

import Html exposing (Html, div, text, form, input, textarea, button, select, option, iframe, a)
import Html.Attributes exposing (value, defaultValue, title, size, cols, rows, type', width, height, src)
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
                , textarea [cols 30, rows 10, defaultValue "text"] []
                , button [value "value"] [text "click me!"]
                , select [] []
                , option [] [text "option a"]
                , iframe [ width 560, height 315, src "https://www.youtube.com/embed/Fg4koastMuM" ] []
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
