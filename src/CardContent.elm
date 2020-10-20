module CardContent exposing (ViewMode(..), Context, view, viewDataOnly, Action(..), Actions, Data, Model, Msg(..), Event(..), encode, decode, update, buttons, emptyModel, event)

import Html exposing (Html, div, text, button, textarea, span)
import Html.Attributes exposing (class, value, placeholder, style, disabled, title)
import Html.Events as HE

import Markdown.Parser as Markdown
import Markdown.Renderer
import Markdown.Html

import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

import Time
import Task

import Calendar
import Calendar.UI
import Utils exposing (..)
import Settings exposing (Settings)

type alias Data =
    { text: String
    , done: Bool
    , calEvents: List Calendar.Event
    }

type alias Model =
    { state: State
    , data: Data
    }

type alias Context =
    { settings: Settings
    }

type State
    = NormalState
    | EditState EditContext

type ViewMode
    = Selected
    | Deselected

type Msg
    = SetDone Bool
    | AddCalEvent
    | AddCalEventWithTime Time.Posix
    | Edit
    | SaveEdit
    | CancelEdit
    | TextChanged String

    | DeleteCalEvent Int

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

view : ViewMode -> Model -> Html Msg
view mode model = case model.state of
    NormalState     -> viewNormal mode model
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

viewNormal : ViewMode -> Model -> Html Msg
viewNormal mode model = viewDataOnly mode model.data

viewDataOnly : ViewMode -> Data -> Html Msg
viewDataOnly mode data = div
    [ class "card-content" ]
    (
    [ viewIndicators mode data
    ] ++
    ( [ case data.text
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
        of
            Ok rendered ->
                div [ class "markdown" ] rendered

            Err errors ->
                div [ class "markdown-errors" ] [ text errors ]

    ] |> detectMaths data.text ) ++
    [ case mode of
        Selected   -> viewCalEvents data.calEvents
        Deselected -> text "" -- fixme
    ])

viewIndicators : ViewMode -> Data -> Html Msg
viewIndicators mode data = let indicatorElements = viewIndicatorElements mode data
    in case (mode, indicatorElements) of
        (_, [])       -> text "" -- fixme
        (Selected, _) -> text ""
        _             -> div [ class "indicators" ] indicatorElements

viewIndicatorElements : ViewMode -> Data -> List (Html Msg)
viewIndicatorElements _ data = case data.calEvents of
    [] -> []
    _  -> [ indicator "indicator-cal-event" "item has a calendar event" ]

indicator : String -> String -> Html Msg
indicator className textContent =
    div
        [ class "indicator", class className, title textContent ]
        [ span [ class "sr-only" ] [ text textContent ] ]

buttons : Model -> Html Msg
buttons model = case model.state of
    NormalState -> viewNormalButtonBar model
    EditState _ -> viewEditButtonBar model

event : Context -> Event -> Model -> (Model, Cmd Msg, Actions)
event ctx evt model = case (evt, model.state) of
    (Destroy, EditState _) -> update ctx SaveEdit model
    (Destroy, _) -> (model, Cmd.none, [])
    (BeginEdit, EditState _) ->
        let (model1, cmd1, actions1) = update ctx SaveEdit model
        in let (model2, cmd2, actions2) = update ctx Edit model
        in (model2, Cmd.batch [ cmd1, cmd2 ], actions1 ++ actions2)
    (BeginEdit, _) -> update ctx Edit model

viewCalEvents : List Calendar.Event -> Html Msg
viewCalEvents cevts = div [ class "calendar-events" ] (List.indexedMap viewCalEvent cevts)

viewCalEvent : Int -> Calendar.Event -> Html Msg
viewCalEvent idx cevt = div [ class "calendar-event" ]
    [ indicator "indicator-cal-event" "calendar event"
    , Calendar.UI.viewEvent cevt
    , button
        [ class "calendar-event-btn" ]
        [ text "edit" ]
    , button
        [ class "calendar-event-btn", onClick (DeleteCalEvent idx) ]
        [ text "delete" ]
    ]

viewNormalButtonBar : Model -> Html Msg
viewNormalButtonBar model = div [ class "button-group", class "button-group-content" ]
    [ button
        [ onClick Edit ]
        [ text "edit" ]
    , button
        [ onClick AddCalEvent ]
        [ text "add event" ]
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

update : Context -> Msg -> Model -> (Model, Cmd Msg, Actions)
update ctx msg model = let data  = model.data
                   in case (msg, model.state) of
    (SetDone done, _)          -> (model |> setData  { data  | done = done }, Cmd.none, [])
    (Edit,         _)          -> (model |> setState (EditState (beginEdit model)), Cmd.none, [])
    (CancelEdit,   _)          -> (model |> setState NormalState, Cmd.none, [])
    (SaveEdit, EditState ectx) -> (model |> setData  { data  | text = ectx.text }
                                         |> setState NormalState, Cmd.none, [])
    (SaveEdit, _)              -> (model |> setState NormalState, Cmd.none, [])
    (TextChanged newText, EditState ectx) ->
        ( model |> setState (EditState { ectx | text = newText }), Cmd.none, [] )
    (TextChanged _, _) -> ( model, Cmd.none, [] )
    (AddCalEvent, _) -> ( model, Task.perform AddCalEventWithTime Time.now, [] )
    (AddCalEventWithTime time, _) ->
        let newEvent = Calendar.defaultEvent ctx.settings.defaultTimezone time
        in
            ( model |> setData { data | calEvents = newEvent :: data.calEvents }
            , Cmd.none
            , []
            )
    (DeleteCalEvent idx, _) -> ( model |> deleteCalEvent idx, Cmd.none, [] )

setData : Data -> Model -> Model
setData data cc = { cc | data = data }

setState : State -> Model -> Model
setState state cc = { cc | state = state }

deleteCalEvent : Int -> Model -> Model
deleteCalEvent idx model = let data = model.data in
    model |> setData { data | calEvents = data.calEvents |> silentDelete idx }


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
decode = JD.succeed Data
    |> JDP.required "text" JD.string
    |> JDP.optional "done" JD.bool False
    |> JDP.optional "cal_events" (JD.list Calendar.decodeEvent) []

encode : Data -> JD.Value
encode content = JE.object
    [ ("text",     JE.string content.text)
    , ("done",     JE.bool   content.done)
    , ("cal_events", JE.list Calendar.encodeEvent content.calEvents)
    ]

silentDelete : Int -> List a -> List a
silentDelete idx l = case (idx, l) of
    (0, (_ :: xs)) -> xs
    (_, [])        -> []
    (_, (x :: xs)) -> x :: (silentDelete (idx - 1) xs)
