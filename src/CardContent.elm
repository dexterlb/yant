module CardContent exposing (view, viewDataOnly, Action(..), Actions, Data, Model, Msg(..), Event(..), encode, decode, update, buttons, emptyModel, event)

import Html exposing (Html, div, text, button, textarea)
import Html.Attributes exposing (class, value, placeholder, style, disabled)
import Html.Events as HE

import Markdown.Parser as Markdown
import Markdown.Renderer
import Markdown.Html

import Json.Encode as JE
import Json.Decode as JD

import Events
import Utils exposing (..)

type alias Data =
    { text: String
    , done: Bool
    }

type alias Model =
    { state: State
    , data: Data
    }

type State
    = NormalState
    | EditState EditContext

type Msg
    = SetDone Bool
    | Edit
    | SaveEdit
    | CancelEdit
    | TextChanged String

type Event
    = Destroy
    | BeginEdit

type Action
    = Dunno

type alias Actions = List Action

type alias EditContext =
    { text: String
    }

emptyModel : Data -> Model
emptyModel data =
    { state = NormalState
    , data  = data
    }

view : Model -> Html Msg
view model = case model.state of
    NormalState     -> viewNormal model
    EditState ectx  -> viewEdit model ectx

viewEdit : Model -> EditContext -> Html Msg
viewEdit model ectx = div
    [ class "card-editing" ]
    [ textarea
        [ value ectx.text
        , placeholder "enter some note text"
        , HE.onInput TextChanged
        ] []
    ]

viewNormal : Model -> Html Msg
viewNormal model = viewDataOnly model.data

viewDataOnly : Data -> Html Msg
viewDataOnly data = div
    [ class "card-content" ]
    (( [ case data.text
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
        of
            Ok rendered ->
                div [ class "markdown" ] rendered

            Err errors ->
                div [ class "markdown-errors" ] [ text errors ]

    ] |> detectMaths data.text ) ++
    [
    ])

buttons : Model -> Html Msg
buttons model = case model.state of
    NormalState -> viewNormalButtonBar model
    EditState _ -> viewEditButtonBar model

event : Event -> Model -> (Model, Actions)
event evt model = case (evt, model.state) of
    (Destroy, EditState _) -> update SaveEdit model
    (Destroy, _) -> (model, [])
    (BeginEdit, EditState _) ->
        let (model1, actions1) = update SaveEdit model
        in let (model2, actions2) = update Edit model
        in (model2, actions1 ++ actions2)
    (BeginEdit, _) -> update Edit model

viewNormalButtonBar : Model -> Html Msg
viewNormalButtonBar model = div [ class "button-group", class "button-group-content" ]
    [ button
        [ onClick Edit ]
        [ text "edit" ]
    , case model.data.done of
        True -> button
            [ onClick <| SetDone False]
            [ text "mark not done" ]
        False -> button
            [ onClick <| SetDone True]
            [ text "mark done" ]
    ]

viewEditButtonBar : Model -> Html Msg
viewEditButtonBar model = div [ class "button-group", class "button-group-content" ]
    [ button
        [ onClick SaveEdit ]
        [ text "save" ]
    , button
        [ onClick CancelEdit ]
        [ text "cancel" ]
    ]

update : Msg -> Model -> (Model, Actions)
update msg model = let data  = model.data
                   in case (msg, model.state) of
    (SetDone done, _)          -> (model |> setData  { data  | done = done }, [])
    (Edit,         _)          -> (model |> setState (EditState (beginEdit model)), [])
    (CancelEdit,   _)          -> (model |> setState NormalState, [])
    (SaveEdit, EditState ectx) -> (model |> setData  { data  | text = ectx.text }
                                         |> setState NormalState, [])
    (SaveEdit, _)              -> (model |> setState NormalState, [])
    (TextChanged newText, EditState ectx) ->
        ( model |> setState (EditState { ectx | text = newText }), [] )
    (TextChanged _, _) -> ( model, [] )

setData : Data -> Model -> Model
setData data cc = { cc | data = data }

setState : State -> Model -> Model
setState state cc = { cc | state = state }


beginEdit : Model -> EditContext
beginEdit model = { text = model.data.text }

deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"

renderer : Markdown.Renderer.Renderer (Html Msg)
renderer = let default = Markdown.Renderer.defaultHtmlRenderer in
    { default
    | html = Markdown.Html.oneOf
        [
        ]
    }

detectMaths : String -> List (Html Msg) -> List (Html Msg)
detectMaths content tags = case hasMaths content of
    True  -> [ renderMaths tags ]
    False -> tags

hasMaths : String -> Bool
hasMaths s = (String.contains "$" s) || (String.contains "\\(" s) || (String.contains "\\[" s)

renderMaths : List (Html Msg) -> Html Msg
renderMaths children = Html.node "may-contain-maths" [ class "maths" ] children

decode : JD.Decoder Data
decode = JD.map2 Data
    (JD.field "text" JD.string)
    (JD.field "done" JD.bool)

encode : Data -> JD.Value
encode content = JE.object
    [ ("text",     JE.string content.text)
    , ("done",     JE.bool   content.done)
    ]
