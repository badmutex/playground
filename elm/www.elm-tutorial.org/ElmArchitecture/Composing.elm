module Main exposing (..)

import Html exposing (Html)
import Html.App
import Widget

type alias AppModel =
    { widgetModel : Widget.Model
    }

initialModel : AppModel
initialModel =
    { widgetModel = Widget.initialModel
    }

init : (AppModel, Cmd Msg)
init =
    (initialModel, Cmd.none)

type Msg
    = WidgetMsg Widget.Msg

view : AppModel -> Html Msg
view model =
    Html.div []
        [ Html.App.map WidgetMsg (Widget.view model.widgetModel)
        ]

update : Msg -> AppModel -> (AppModel, Cmd Msg)
update message model =
    case message of
        WidgetMsg subMsg ->
            let
                (updatedWidgetModel, widgetCmd) =
                    Widget.update subMsg model.widgetModel
            in
                ( { model | widgetModel = updatedWidgetModel }, Cmd.map WidgetMsg widgetCmd )


subscriptions : AppModel -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
