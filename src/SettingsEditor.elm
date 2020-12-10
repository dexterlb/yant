module SettingsEditor exposing (Model, Msg, init, update, view, getSettings)

import Settings as Settings exposing (Settings, SyncSettings)
import Html as HI exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

import Utils exposing (..)
import CommonComponents exposing (viewTZPicker)

type alias Model =
    { settings: Settings
    }

type Msg
    = Evil (Model -> Model)

init : Settings -> Model
init s = { settings = s }

getSettings : Model -> Settings
getSettings { settings } = settings

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Evil f -> (f model, Cmd.none)

view : Model -> Html Msg
view model = let s = model.settings in HI.div []
    [ checkbox s.hideDone
        [ onClick (Evil (\m -> { m | settings = { s | hideDone = not s.hideDone } }))
        , HA.class "setting" ]
        [ HI.text "hide done items" ]
    , HI.div [ HA.class "setting" ]
        [ HI.label [] [ HI.text "default timezone" ]
        , viewTZPicker s.defaultTimezone (\v -> Evil (\m -> { m | settings = { s | defaultTimezone = v } }))
        ]
    , viewSyncSettingsEditor s.sync (\ss m -> { m | settings = { s | sync = ss }})
    ]

viewSyncSettingsEditor : SyncSettings -> (SyncSettings -> Model -> Model) -> Html Msg
viewSyncSettingsEditor ss f = HI.div [ HA.class "ss-box" ]
    [ HI.label [] [ HI.text "sync ss" ]

    , HI.div [ HA.class "setting" ]
        [ HI.label [] [ HI.text "server" ]
        , HI.input
            [ HA.type_ "text"
            , HA.value ss.server
            , HE.onInput (\v -> Evil (f { ss | server = v }))
            ] []
        ]

    , HI.div [ HA.class "setting" ]
        [ HI.label [] [ HI.text "space" ]
        , HI.input
            [ HA.type_ "text"
            , HA.value ss.space
            , HE.onInput (\v -> Evil (f { ss | space = v }))
            ] []
        ]

    , HI.div [ HA.class "setting" ]
        [ HI.label [] [ HI.text "secret" ]
        , HI.input
            [ HA.type_ "text"
            , HA.value ss.secret
            , HE.onInput (\v -> Evil (f { ss | secret = v }))
            ] []
        ]

    , checkbox ss.enabled
        [ onClick (Evil (f { ss | enabled = not ss.enabled }))
        , HA.class "setting" ]
        [ HI.text "enabled" ]
    ]
