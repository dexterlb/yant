module Calendar.EventEditor exposing (Model, Msg, init, update, view, getEvent)

import Html exposing (Html, div, pre, text, button, textarea, input)
import Html.Attributes exposing (class, value, placeholder, style, disabled, type_, checked)
import Html.Events as HE

import Json.Encode as JE

import Calendar exposing (..)

type alias Model =
    { event: Event
    }

type Msg
    = Foo

init : Event -> Model
init evt = { event = evt }

getEvent : Model -> Event
getEvent { event } = event

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Foo -> (model, Cmd.none)

view : Model -> Html Msg
view model = text "editor goes here"
