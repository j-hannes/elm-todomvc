module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ header [ class "header" ]
            [ h1 []
                [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , value ""
                ]
                []
            ]
        , section [ class "main" ]
            [ input [ class "toggle-all", type_ "checkbox" ]
                []
            , ul [ class "todo-list" ]
                [ li [ class "completed" ]
                    [ div [ class "view" ]
                        [ input [ class "toggle", type_ "checkbox" ]
                            []
                        , label []
                            [ text "buy some milk" ]
                        , button [ class "destroy" ]
                            []
                        ]
                    , input [ class "edit", value "get some milk" ]
                        []
                    ]
                , li [ class "" ]
                    [ div [ class "view" ]
                        [ input [ class "toggle", type_ "checkbox" ]
                            []
                        , label []
                            [ text "empty bins" ]
                        , button [ class "destroy" ]
                            []
                        ]
                    , input [ class "edit", value "empty bind" ]
                        []
                    ]
                ]
            ]
        , footer [ class "footer" ]
            [ span [ class "todo-count" ]
                [ strong []
                    [ text "1" ]
                , span []
                    [ text " " ]
                , span []
                    [ text "item" ]
                , span []
                    [ text " left" ]
                ]
            , ul [ class "filters" ]
                [ li []
                    [ a [ class "selected", href "#/" ]
                        [ text "All" ]
                    ]
                , span []
                    []
                , li []
                    [ a [ class "", href "#/active" ]
                        [ text "Active" ]
                    ]
                , span []
                    []
                , li []
                    [ a [ class "", href "#/completed" ]
                        [ text "Completed" ]
                    ]
                ]
            , button [ class "clear-completed" ]
                [ text "Clear completed" ]
            ]
        ]
