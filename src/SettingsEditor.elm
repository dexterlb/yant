module SettingsEditor exposing (Model, Msg, init, update, view, getSettings)

import Settings as Settings exposing (Settings)
import Html as HI exposing (Html)

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
        [ onClick (Evil (\m -> { m | settings = { s | hideDone = not s.hideDone } })) ]
        [ HI.text "hide done items" ]
    , HI.div []
        [ HI.label [] [ HI.text "default timezone" ]
        , viewTZPicker s.defaultTimezone (\v -> Evil (\m -> { m | settings = { s | defaultTimezone = v } }))
        ]
    ]

