module Calendar.EventEditor exposing (Model, Msg, init, update, view, getEvent)

import Html exposing (Html, div, pre, text, button, textarea, input, label, select, option, optgroup, span)
import Html.Attributes exposing (class, value, placeholder, style, disabled, type_, checked, step, required, selected, attribute)
import Html.Events as HE

import Json.Encode as JE

import Calendar.DateTimeFormats exposing (..)
import Calendar exposing (..)

import Calendar.Timezones as Timezones exposing (Timezone)

import Utils exposing (..)

type alias Model =
    { event: Event
    }

type Msg
    = Evil (Model -> Model)

init : Event -> Model
init evt = { event = evt }

getEvent : Model -> Event
getEvent { event } = event

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Evil f -> (fixup (f model), Cmd.none)

view : Model -> Html Msg
view model = let event = model.event in div
    [ class "cal-event-editor" ]
    [ div
        [ class "start-end" ]
        [ div
            [ class "dtp-group" ]
            [ label [] [ text "at: " ]
            , viewDTPicker event.start (\dt m -> { m | event = { event | start = dt } })
            ]
        , div
            [ class "dtp-group" ]
            [ label [] [ text "end: " ]
            , case event.end of
                Nothing ->
                    checkbox
                        [ checked False
                        , HE.onClick (Evil (\m -> { m | event = { event | end = Just event.start, kind = CalendarEvent } }))
                        ] 
                        []
                Just end -> span []
                    [ checkbox
                        [ checked True
                        , HE.onClick (Evil (\m -> { m | event = { event | end = Nothing, kind = Task } }))
                        ] []
                    , viewDTPicker end (\dt m -> { m | event = { event | end = Just dt } })
                    ]
            ]
        , div
            [ class "dtp-group" ]
            [ checkbox 
                [ checked event.allDay 
                , onClick (Evil (\m -> { m | event = { event | allDay = not event.allDay } } ) )
                ] 
                [ text "all day" ]
            , checkbox 
                [ checked event.busy 
                , onClick (Evil (\m -> { m | event = { event | busy = not event.busy } } ) )
                ] 
                [ text "treat as busy" ]
            , select
                [ HE.onInput (\k -> (Evil (\m -> { m | event = { event | kind = parseKind k event.kind } } ) ) )
                ]
                (List.map (kindOption model.event.kind) [ Task, CalendarEvent ])
            ]
        , div
            [ class "reminder-list" ]
            [ label [] [ text "reminders: " ]
            , viewReminderEditors model
            , button
                [ class "calendar-event-btn"
                , class "add-reminder-btn"
                , onClick (Evil (\m -> { m | event = { event | reminders = defaultReminder :: event.reminders } })) ]
                [ text "add reminder" ]
            ]
        ]
    ]

fixup : Model -> Model
fixup model = let event = model.event in model
    |> (\m -> case m.event.allDay of
            True -> { m | event = { event | start = setTime (0, 0, 0) event.start 
                                          , end = Maybe.map (setTime (23, 59, 59)) event.end } }
            False -> m)

viewReminderEditors : Model -> Html Msg
viewReminderEditors model = div [ class "reminder-editors" ]
    (List.indexedMap (viewReminderEditor model) model.event.reminders)

viewReminderEditor : Model -> Int -> Reminder-> Html Msg
viewReminderEditor model index rem = let event = model.event in div [ class "reminder-editor" ]
    [ indicator "indicator-reminder" "reminder"
    , viewSignedDurationPicker rem.trigger (\dur m -> setReminder m index { rem | trigger = dur })
    , viewReminderRepeatPicker rem.repeat  (\rep m -> setReminder m index { rem | repeat  = rep })
    , viewNoisinessPicker      rem.noisiness (\n m -> setReminder m index { rem | noisiness = n })
    , button
        [ class "calendar-event-btn"
        , class "add-reminder-btn"
        , onClick (Evil (\m -> { m | event = { event | reminders = event.reminders |> silentDelete index } })) ]
        [ text "delete" ]
    ]

setReminder : Model -> Int -> Reminder -> Model
setReminder model index rem = let event = model.event in 
    { model | event = { event | reminders = silentUpdate index rem event.reminders } }

viewReminderRepeatPicker : ReminderRepeat -> (ReminderRepeat -> Model -> Model) -> Html Msg
viewReminderRepeatPicker rr f = div [ class "reminder-repeat-picker" ]
    [ text "repeat"
    , input
        [ type_ "number"
        , value (String.fromInt <| Calendar.timesOf rr)
        , HE.onInput (\v -> Evil (f (
            case String.toInt v of
                Nothing -> rr
                Just n  ->
                    case n <= 1 of
                        True  -> NoRepeat
                        False -> RepeatAt n (Calendar.intervalOf rr)
            )))
        ] []
    , case rr of
        NoRepeat -> span [] [ text "times" ]
        RepeatAt n int -> span []
            [ text "times, spaced at"
            , viewDurationPicker int (\dur -> f (RepeatAt n dur))
            ]
    ]

viewNoisinessPicker : Noisiness -> (Noisiness -> Model -> Model) -> Html Msg
viewNoisinessPicker n f = select
    [ class "noisiness-picker" 
    , HE.onInput (\ns -> Evil (f (case ns of
        "noisy"  -> Noisy
        "silent" -> Silent
        _        -> Noisy
        )))
    ]
    [ option [ value "noisy",  selected (n == Noisy) ]  [ text "noisy" ]
    , option [ value "silent", selected (n == Silent) ] [ text "silent" ]
    ]

viewSignedDurationPicker : SignedDuration -> (SignedDuration -> Model -> Model) -> Html Msg
viewSignedDurationPicker dur f = div
    [ class "signed-duration-picker" ]
    [ case dur <= 0 of
        True  -> viewDurationPicker (0 - dur) (\newDur m -> f (0 - newDur) m)
        False -> viewDurationPicker    dur  (\newDur m -> f newDur    m)
    , case dur == 0 of
        True  -> text "at the moment of the event"
        False -> select
            [ class "duration-sign"
            , HE.onInput (\dsn -> Evil ( f (case dsn of
                "before" -> 0 - (abs dur)
                "after"  -> 0 + (abs dur)
                _        -> dur
                    )))
            ]
            [ option [ value "before", selected (dur < 0) ] [ text "Before" ]
            , option [ value "after",  selected (dur > 0) ] [ text "After"  ]
            ]
    ]

viewDurationPicker : Duration -> (Duration -> Model -> Model) -> Html Msg
viewDurationPicker dur f = div
    [ class "duration-picker" ]
    
    [ viewDurationMultiplePicker 86400     0  dur f, text "days"
    , viewDurationMultiplePicker 3600      24 dur f, text "hr"
    , viewDurationMultiplePicker 60        60 dur f, text "min"
    , viewDurationMultiplePicker 1         60 dur f, text "sec"
    ]

viewDurationMultiplePicker : Int -> Int -> Duration -> (Duration -> Model -> Model) -> Html Msg
viewDurationMultiplePicker quot rem dur f =
    let current = case rem of
                    0 ->            dur // quot
                    _ -> modBy rem (dur // quot) 
    in input
        [ class "duration-multiple", type_ "number"
        , value (String.fromInt current)
        , HE.onInput (\v -> case String.toInt v of
            Just new -> Evil (f (dur - current * quot + new * quot))
            Nothing  -> Evil (f dur))
        ] []

viewDTPicker : DateTime -> (DateTime -> Model -> Model) -> Html Msg
viewDTPicker dt f = div
    [ class "date-picker" ]
    [ input
        [ class "date", type_ "date", value (dateToValue dt), required True
        , HE.onInput (\v -> Evil (f (parseDatePart v dt)))] []
    , input
        [ class "time", type_ "time", value (timeToValue dt), step "1", required True
        , HE.onInput (\v -> Evil (f (parseTimePart v dt)))] []
    , viewTZPicker dt.timezone (\v -> f { dt | timezone = v })
    ]

viewTZPicker : Timezone -> (Timezone -> Model -> Model) -> Html Msg
viewTZPicker tz f =
    select
        [ class "timezone"
        , HE.onInput (\tzn -> case Timezones.fromString tzn of
            Just newTz -> Evil (f newTz)
            Nothing    -> Evil (f tz))
        ]
        (List.map (tzOptionGroup tz) (Timezones.grouped Timezones.all))

tzOptionGroup : Timezone -> (String, List Timezone) -> Html Msg
tzOptionGroup selectedTZ (groupName, tzs) =
    optgroup
        [ attribute "label" groupName ]
        (List.map (tzOption selectedTZ) tzs)

tzOption : Timezone -> Timezone -> Html Msg
tzOption selectedTZ tz = let tzn = Timezones.toString tz in
    case tzn == Timezones.toString selectedTZ of
        True  -> option [ value tzn, selected True  ] [ text tzn ]
        False -> option [ value tzn, selected False ] [ text tzn ]

parseKind : String -> Kind -> Kind
parseKind s k = Maybe.withDefault k (kindFromString s)

kindOption : Kind -> Kind -> Html Msg
kindOption selectedK k = option [ value (kindToString k), selected (k == selectedK) ] [ text (kindName k) ]

setTime : (Int, Int, Int) -> DateTime -> DateTime
setTime (h, m, s) dt = { dt | hour = h, minute = m, second = s }
