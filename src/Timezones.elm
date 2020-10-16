module Timezones exposing ( Timezone, allTimezones, encodeTimezone, decodeTimezone
                          , timezoneToString, timezoneFromString )

import Time
import TimeZone

import Dict
import Utils exposing (..)

import Json.Encode as JE
import Json.Decode exposing (Decoder)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

type Timezone = Timezone String Time.Zone

allTimezones : List Timezone
allTimezones = TimeZone.zones
    |> Dict.toList
    |> List.map (\(name, f) -> Timezone name (f ()))

encodeTimezone : Timezone -> JE.Value
encodeTimezone tz = JE.string (timezoneToString tz)

decodeTimezone : Decoder Timezone
decodeTimezone = JD.string
    |> JD.map timezoneFromString
    |> decodeOrFail "invalid timezone"

timezoneToString : Timezone -> String
timezoneToString (Timezone name _) = name

timezoneFromString : String -> Maybe Timezone
timezoneFromString name = Dict.get name TimeZone.zones
    |> Maybe.map (\f -> Timezone name (f ()))
