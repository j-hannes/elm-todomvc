module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onEnter)
import Maybe exposing (map, withDefault)
import Navigation


---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Model =
    { todos : List Todo
    , nextTodoId : TodoId
    , newTodo : String
    , todoFilter : TodoFilter
    , editing : Maybe TodoId
    }


type alias Todo =
    { id : TodoId
    , description : String
    , completed : Bool
    }


type alias TodoId =
    Int


type TodoFilter
    = All
    | Active
    | Completed


initialModel : Navigation.Location -> Model
initialModel location =
    { todos = []
    , nextTodoId = 1
    , newTodo = ""
    , todoFilter = All
    , editing = Nothing
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel location, Cmd.none )



---- UPDATE ----


type Msg
    = CheckTodo TodoId
    | UpdateNewTodo String
    | AddTodo
    | DeleteTodo TodoId
    | EditTodo TodoId
    | UpdateEditingTodo String
    | FinishEditing
    | UrlChange Navigation.Location
    | ClearCompleted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckTodo id ->
            updateModel <| checkTodo id model

        UpdateNewTodo newTodo ->
            updateModel <| setNewTodo newTodo model

        AddTodo ->
            updateModel <| addTodo model

        DeleteTodo id ->
            updateModel <| deleteTodo id model

        EditTodo id ->
            updateModel <| setEditing id model

        UpdateEditingTodo description ->
            updateModel <| updateEditing description model

        FinishEditing ->
            updateModel <| finishEditing model

        UrlChange location ->
            updateModel <| updateFilter location.hash model

        ClearCompleted ->
            updateModel <| clearCompleted model


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


setNewTodo : String -> Model -> Model
setNewTodo newTodo model =
    { model | newTodo = newTodo }


addTodo : Model -> Model
addTodo model =
    let
        description =
            String.trim model.newTodo

        nextTodo =
            Todo model.nextTodoId description False
    in
        if String.length description == 0 then
            model
        else
            { model
                | todos = nextTodo :: model.todos
                , nextTodoId = model.nextTodoId + 1
                , newTodo = ""
            }


deleteTodo : TodoId -> Model -> Model
deleteTodo id model =
    let
        filter =
            List.filter (\todo -> todo.id /= id)
    in
        { model | todos = filter model.todos }


setEditing : TodoId -> Model -> Model
setEditing id model =
    { model | editing = Just id }


updateEditing : String -> Model -> Model
updateEditing description model =
    let
        updateTodos : Todo -> Todo
        updateTodos todo =
            if Just todo.id == model.editing then
                { todo | description = description }
            else
                todo
    in
        { model
            | todos = List.map updateTodos model.todos
        }


finishEditing : Model -> Model
finishEditing model =
    { model | editing = Nothing }


updateFilter : String -> Model -> Model
updateFilter hash model =
    let
        todoFilter =
            if hash == "#/" then
                All
            else if hash == "#/active" then
                Active
            else if hash == "#/completed" then
                Completed
            else
                model.todoFilter
    in
        { model | todoFilter = todoFilter }


clearCompleted : Model -> Model
clearCompleted model =
    { model | todos = List.filter (\todo -> not todo.completed) model.todos }



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
                , value model.newTodo
                , onInput UpdateNewTodo
                , onEnter AddTodo
                ]
                []
            ]
        , section [ class "main" ]
            [ input [ class "toggle-all", type_ "checkbox" ]
                []
            , ul [ class "todo-list" ]
                (List.map (viewTodo model.editing) <| filterTodos model.todoFilter model.todos)
            ]
        , footer [ class "footer" ]
            [ viewTodoCount model.todos
            , viewFilters model
            , button
                [ class "clear-completed"
                , onClick ClearCompleted
                ]
                [ text "Clear completed" ]
            ]
        ]


filterTodos : TodoFilter -> List Todo -> List Todo
filterTodos todoFilter todos =
    case todoFilter of
        All ->
            todos

        Active ->
            List.filter (\todo -> not todo.completed) todos

        Completed ->
            List.filter (\todo -> todo.completed) todos


countIncomplete : List Todo -> Int
countIncomplete todos =
    List.length <| List.filter (\todo -> not todo.completed) todos


viewTodo : Maybe TodoId -> Todo -> Html Msg
viewTodo editing todo =
    li
        [ classList
            [ ( "completed", todo.completed )
            , ( "editing", editing == Just todo.id )
            ]
        ]
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick <| CheckTodo todo.id
                ]
                []
            , label
                [ onDoubleClick <| EditTodo todo.id
                ]
                [ text todo.description
                ]
            , button
                [ class "destroy"
                , onClick <| DeleteTodo todo.id
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.description
            , onInput UpdateEditingTodo
            , onBlur FinishEditing
            ]
            []
        ]


viewTodoCount : List Todo -> Html Msg
viewTodoCount todos =
    let
        incompleteTodoCount =
            countIncomplete todos

        itemLabel =
            if incompleteTodoCount == 1 then
                "item"
            else
                "items"
    in
        span [ class "todo-count" ]
            [ strong []
                [ text <| toString incompleteTodoCount ]
            , span []
                [ text " " ]
            , span []
                [ text itemLabel ]
            , span []
                [ text " left" ]
            ]


viewFilters : Model -> Html Msg
viewFilters model =
    let
        filters =
            [ ( All, "#/" )
            , ( Active, "#/active" )
            , ( Completed, "#/completed" )
            ]
    in
        ul [ class "filters" ] <|
            List.intersperse (span [] []) <|
                List.map (viewFilter model) filters


viewFilter : Model -> ( TodoFilter, String ) -> Html Msg
viewFilter model ( todoFilter, path ) =
    let
        className =
            (if model.todoFilter == todoFilter then
                "selected"
             else
                ""
            )
    in
        li []
            [ a
                [ class className
                , href path
                ]
                [ text <| toString todoFilter ]
            ]
