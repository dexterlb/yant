module Calendar.UI exposing (viewEvent)

import Html exposing (Html, div, pre, text, button, textarea, input)
import Html.Attributes exposing (class, value, placeholder, style, disabled, type_, checked)
import Html.Events as HE

import Json.Encode as JE

import Calendar exposing (..)

viewEvent : Event -> Html msg
viewEvent evt = pre [ class "event-content" ] [ text <| JE.encode 0 (encodeEvent evt) ]
