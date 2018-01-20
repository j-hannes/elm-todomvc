module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
    { todos : List Todo
    }


type alias Todo =
    { id : TodoId
    , description : String
    , completed : Bool
    }


type alias TodoId =
    Int


initialModel : Model
initialModel =
    { todos =
        [ Todo 1 "buy some milk" True
        , Todo 2 "empty bins" False
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = CheckTodo TodoId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckTodo id ->
            updateModel <| checkTodo id model


updateModel : Model -> ( Model, Cmd Msg )
updateModel model =
    ( model, Cmd.none )


checkTodo : TodoId -> Model -> Model
checkTodo id model =
    let
        check todo =
            if todo.id == id then
                { todo | completed = not todo.completed }
            else
                todo
    in
        { model | todos = List.map check model.todos }



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
                (List.map
                    viewTodo
                    model.todos
                )
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


viewTodo : Todo -> Html Msg
viewTodo todo =
    li
        [ class <|
            if todo.completed then
                "completed"
            else
                ""
        ]
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick <| CheckTodo todo.id
                ]
                []
            , label []
                [ text todo.description ]
            , button [ class "destroy" ]
                []
            ]
        , input [ class "edit", value todo.description ]
            []
        ]
