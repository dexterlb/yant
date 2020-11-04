module CardContent exposing (Context, view, viewDataOnly, Action(..), Actions, Data, Model, Msg(..), Event(..), encode, decode, update, buttons, emptyModel, event)

import Html exposing (Html, div, text, button, textarea, span, a)
import Html.Attributes exposing (class, value, placeholder, style, disabled, title, href)
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
import Calendar.EventViewer
import Calendar.EventEditor
import Utils exposing (..)
import Settings exposing (Settings)

type alias Data =
    { text: String
    , done: Bool
    , calEvents: List Calendar.Event
    , attachedFiles: List AttachedFile
    }

type alias Model =
    { state: State
    , data: Data
    }

type alias Context =
    { settings: Settings
    }

type alias AttachedFile =
    { name: String
    , hash: String
    }

type State
    = NormalState
    | EditState EditContext
    | EditCalEventState EditCalEventContext
    | EditAttachedFileState EditAttachedFileContext

type Msg
    = SetDone Bool
    | Edit
    | SaveEdit
    | Cancel
    | TextChanged String

    | CalEventEditorMsg Calendar.EventEditor.Msg

    | AddCalEvent
    | AddCalEventWithTime Time.Posix
    | EditCalEvent Calendar.Event Int
    | DeleteCalEvent Int
    | SaveCalEvent

    | AttachFile
    | EditAttachedFile AttachedFile Int
    | SaveAttachedFile
    | DetachFile Int
    | DownloadAttachedFile AttachedFile

type Event
    = Destroy
    | BeginEdit
    | ReceiveAttachedFile AttachedFile

type Action
    = RequestAttachedFile
    | RequestAttachedFileDownload AttachedFile

type alias Actions = List Action

type alias EditContext =
    { text: String
    }

type alias EditCalEventContext =
    { calEventIndex: Int
    , editor: Calendar.EventEditor.Model
    }

type alias EditAttachedFileContext =
    { fileIndex: Int
    , file: AttachedFile
    }

emptyModel : Data -> Model
emptyModel data =
    { state = NormalState
    , data  = data
    }

view : Model -> Html Msg
view model = case model.state of
    NormalState             -> viewNormal model
    EditCalEventState _     -> viewNormal model
    EditAttachedFileState _ -> viewNormal model
    EditState ectx          -> viewEdit model ectx

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
viewNormal model = div [ class "card-content" ]
    [ viewBody model.data
    , case model.state of
        EditCalEventState ectx -> viewCalEvents (Just ectx) model.data.calEvents
        _                      -> viewCalEvents Nothing     model.data.calEvents
    , case model.state of
        EditAttachedFileState ectx -> viewAttachedFiles (Just ectx) model.data.attachedFiles
        _                          -> viewAttachedFiles Nothing     model.data.attachedFiles
    ]

viewDataOnly : Data -> Html Msg
viewDataOnly data = div
    [ class "card-content" ]
    [ viewIndicators data
    , viewBody data
    ]

viewBody : Data -> Html Msg
viewBody data = div [ class "card-body" ]
    [ ( case data.text
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)
        of
            Ok rendered ->
                div [ class "markdown" ] rendered

            Err errors ->
                div [ class "markdown-errors" ] [ text errors ]

      ) |> detectMaths data.text
    ]

viewIndicators : Data -> Html Msg
viewIndicators data = let indicatorElements = viewIndicatorElements data
    in case indicatorElements of
        [] -> text "" -- fixme
        _  -> div [ class "indicators" ] indicatorElements

viewIndicatorElements : Data -> List (Html Msg)
viewIndicatorElements data = case data.calEvents of
    [] -> []
    _  -> [ indicator "indicator-cal-event" "item has a calendar event" ]
          ++ (case (List.any Calendar.hasReminders data.calEvents) of
                True  -> [ indicator "indicator-reminder" "item has a reminder" ]
                False -> [])

buttons : Model -> Html Msg
buttons model = case model.state of
    NormalState             -> viewNormalButtonBar model
    EditState _             -> viewEditButtonBar model
    EditCalEventState _     -> viewCancelButtonBar model
    EditAttachedFileState _ -> viewCancelButtonBar model

event : Context -> Event -> Model -> (Model, Cmd Msg, Actions)
event ctx evt model = let data = model.data in case (evt, model.state) of
    (Destroy, EditState _) -> update ctx SaveEdit model
    (Destroy, _) -> (model, Cmd.none, [])
    (BeginEdit, EditState _) ->
        let (model1, cmd1, actions1) = update ctx SaveEdit model
        in let (model2, cmd2, actions2) = update ctx Edit model
        in (model2, Cmd.batch [ cmd1, cmd2 ], actions1 ++ actions2)
    (BeginEdit, _) -> update ctx Edit model
    (ReceiveAttachedFile af, _) ->
        ( { model | data = { data | attachedFiles = af :: data.attachedFiles } }
        , Cmd.none
        , [] )

viewCalEvents : (Maybe EditCalEventContext) -> List Calendar.Event -> Html Msg
viewCalEvents mectx cevts =
    let viewOrEdit = case mectx of
                        Nothing -> viewCalEvent
                        Just { calEventIndex, editor } ->
                            (\idx cevt -> case idx == calEventIndex of
                                True -> viewCalEventEditor idx editor
                                False -> viewCalEvent idx cevt)
    in
        div [ class "calendar-events-outer" ]
            [ div [ class "calendar-events" ] (List.indexedMap viewOrEdit cevts)
            ]

viewCalEvent : Int -> Calendar.Event -> Html Msg
viewCalEvent idx cevt = div [ class "calendar-event" ]
    [ indicator "indicator-cal-event" "calendar event"
    , Calendar.EventViewer.view cevt
    , button
        [ class "calendar-event-btn", onClick (EditCalEvent cevt idx) ]
        [ text "edit" ]
    , button
        [ class "calendar-event-btn", onClick (DeleteCalEvent idx) ]
        [ text "delete" ]
    ]

viewCalEventEditor : Int -> Calendar.EventEditor.Model -> Html Msg
viewCalEventEditor idx cevt = div [ class "calendar-event" ]
    [ indicator "indicator-cal-event" "calendar-event"
    , Calendar.EventEditor.view cevt
      |> Html.map CalEventEditorMsg
    , button
        [ class "calendar-event-btn", onClick SaveCalEvent ]
        [ text "save" ]
    , button
        [ class "calendar-event-btn", onClick Cancel ]
        [ text "cancel" ]
    ]

viewAttachedFiles : (Maybe EditAttachedFileContext) -> List AttachedFile -> Html Msg
viewAttachedFiles mectx attachedFiles =
    let viewOrEdit = case mectx of
                        Nothing -> viewAttachedFile
                        Just { fileIndex, file } ->
                            (\idx af -> case idx == fileIndex of
                                True -> viewAttachedFileEditor idx file
                                False -> viewAttachedFile idx af)
    in
        div [ class "attached-files-outer" ]
            [ div [ class "attached-files" ] (List.indexedMap viewOrEdit attachedFiles)
            ]

viewAttachedFile : Int -> AttachedFile -> Html Msg
viewAttachedFile idx af = div [ class "attached-file" ]
    [ indicator "indicator-attached-file" "attached file"
    , viewAttachedFileLink af
    , button
        [ class "attach-file-btn", onClick (EditAttachedFile af idx) ]
        [ text "edit" ]
    , button
        [ class "attach-file-btn", onClick (DetachFile idx) ]
        [ text "detach" ]
    ]

viewAttachedFileLink : AttachedFile -> Html Msg
viewAttachedFileLink af = a
    [ href "#"
    , onClick (DownloadAttachedFile af)
    ]
    [ text af.name ]

viewAttachedFileEditor : Int -> AttachedFile -> Html Msg
viewAttachedFileEditor idx af = div [ class "attached-file" ]
    [ indicator "indicator-attached-file" "attached file"
    , viewAttachedFileEditorBody af
    , button
        [ class "attached-file-btn", onClick SaveAttachedFile ]
        [ text "save" ]
    , button
        [ class "attached-file-btn", onClick Cancel ]
        [ text "cancel" ]
    ]

viewAttachedFileEditorBody : AttachedFile -> Html Msg
viewAttachedFileEditorBody af = div [ class "attached-file-editor" ]
    [ text "<editor goes here>" ]


viewNormalButtonBar : Model -> Html Msg
viewNormalButtonBar model = div [ class "button-group", class "button-group-content" ]
    [ button
        [ onClick Edit ]
        [ text "edit" ]
    , button
        [ onClick AddCalEvent ]
        [ text "add event" ]
    , button
        [ onClick AttachFile ]
        [ text "attach file" ]
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
        [ onClick Cancel ]
        [ text "cancel" ]
    ]

viewCancelButtonBar : Model -> Html Msg
viewCancelButtonBar model = div [ class "button-group", class "button-group-content" ]
    [ button
        [ onClick Cancel ]
        [ text "cancel" ]
    ]

update : Context -> Msg -> Model -> (Model, Cmd Msg, Actions)
update ctx msg model = let data  = model.data
                   in case (msg, model.state) of
    (SetDone done, _)          -> (model |> setData  { data  | done = done }, Cmd.none, [])
    (Edit,         _)          -> (model |> setState (EditState (beginEdit model)), Cmd.none, [])
    (Cancel,       _)          -> (model |> setState NormalState, Cmd.none, [])
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
            ( model
                |> setData { data | calEvents = newEvent :: data.calEvents }
                |> editCalEvent newEvent 0
            , Cmd.none
            , []
            )
    (DeleteCalEvent idx, _) -> ( model |> deleteCalEvent idx, Cmd.none, [] )
    (EditCalEvent cevt idx, _) -> ( model |> editCalEvent cevt idx, Cmd.none, [] )

    (CalEventEditorMsg cemsg, EditCalEventState cectx) ->
        let (newEditor, cecmd) = Calendar.EventEditor.update cemsg cectx.editor
        in
            ( model |> setState (EditCalEventState { cectx | editor = newEditor })
            , Cmd.map CalEventEditorMsg cecmd
            , []
            )
    (SaveCalEvent, EditCalEventState cectx) ->
        ( model |> updateCalEvent cectx.calEventIndex (Calendar.EventEditor.getEvent cectx.editor)
                |> setState NormalState
        , Cmd.none
        , []
        )

    (CalEventEditorMsg _, _) -> (model, Cmd.none, [])
    (SaveCalEvent, _) -> (model, Cmd.none, [])

    (AttachFile, _) -> (model, Cmd.none, [ RequestAttachedFile ])
    (DetachFile idx, _)          -> ( model |> deleteAttachedFile  idx, Cmd.none, [] )
    (EditAttachedFile af idx, _) -> ( model |> editAttachedFile af idx, Cmd.none, [] )
    (SaveAttachedFile, EditAttachedFileState afectx) ->
        ( model |> updateAttachedFile afectx.fileIndex afectx.file |> setState NormalState
        , Cmd.none
        , []
        )
    (DownloadAttachedFile af, _) -> ( model, Cmd.none, [ RequestAttachedFileDownload af ] )
    (SaveAttachedFile, _) -> ( model, Cmd.none, [] )

setData : Data -> Model -> Model
setData data cc = { cc | data = data }

setState : State -> Model -> Model
setState state cc = { cc | state = state }

deleteCalEvent : Int -> Model -> Model
deleteCalEvent idx model = let data = model.data in
    model |> setData { data | calEvents = data.calEvents |> silentDelete idx }

editCalEvent : Calendar.Event -> Int -> Model -> Model
editCalEvent cevt idx model = let data = model.data in
    model |> setState (EditCalEventState
        { calEventIndex = idx, editor = Calendar.EventEditor.init cevt })

updateCalEvent : Int -> Calendar.Event -> Model -> Model
updateCalEvent idx cevt model = let data = model.data in
    model |> setData { data | calEvents = silentUpdate idx cevt data.calEvents }

deleteAttachedFile : Int -> Model -> Model
deleteAttachedFile idx model = let data = model.data in
    model |> setData { data | attachedFiles = data.attachedFiles |> silentDelete idx }

editAttachedFile : AttachedFile -> Int -> Model -> Model
editAttachedFile af idx model = let data = model.data in
    model |> setState (EditAttachedFileState
        { fileIndex = idx, file = af })

updateAttachedFile : Int -> AttachedFile -> Model -> Model
updateAttachedFile idx af model = let data = model.data in
    model |> setData { data | attachedFiles = silentUpdate idx af data.attachedFiles }


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

detectMaths : String -> Html Msg -> Html Msg
detectMaths content tag = case hasMaths content of
    True  -> renderMaths [ tag ]
    False -> tag

hasMaths : String -> Bool
hasMaths s = (String.contains "$" s) || (String.contains "\\(" s) || (String.contains "\\[" s)

renderMaths : List (Html Msg) -> Html Msg
renderMaths children = Html.node "may-contain-maths" [ class "maths" ] children

decode : JD.Decoder Data
decode = JD.succeed Data
    |> JDP.required "text" JD.string
    |> JDP.optional "done" JD.bool False
    |> JDP.optional "cal_events" (JD.list Calendar.decodeEvent) []
    |> JDP.optional "attached_files" (JD.list decodeAttachedFile) []

encode : Data -> JE.Value
encode content = JE.object
    [ ("text",     JE.string content.text)
    , ("done",     JE.bool   content.done)
    , ("cal_events", JE.list Calendar.encodeEvent content.calEvents)
    , ("attached_files", JE.list encodeAttachedFile content.attachedFiles)
    ]

decodeAttachedFile : JD.Decoder AttachedFile
decodeAttachedFile = JD.succeed AttachedFile
    |> JDP.required "name" JD.string
    |> JDP.required "hash" JD.string

encodeAttachedFile : AttachedFile -> JE.Value
encodeAttachedFile af = JE.object
    [ ("name", JE.string af.name)
    , ("hash", JE.string af.hash)
    ]
