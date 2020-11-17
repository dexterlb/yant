module SettingsEditor exposing (Model, Msg, init, update, view, getSettings)

import Settings as Settings exposing (Settings)
import Html as HI exposing (Html)
import Utils exposing (..)

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
view model = HI.div []
    [ HI.text "this is the settings editor" ]
