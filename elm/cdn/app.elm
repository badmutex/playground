
import CDN exposing (bootstrap)
import Html
import Html.Attributes exposing (..)
import Html.App

-- MODEL

type Model
    = Empty

init : (Model, Cmd Msg)
init =
    (Empty, Cmd.none)


-- MESSAGES

type Msg
    = NoOp


-- VIEW

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ bootstrap.css
        , Html.body [] [ Html.h1 [] [Html.text "H1!"]
                       , Html.p [] [Html.text "paragraph here one two three"]
                       ]
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
