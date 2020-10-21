module Timezones exposing ( Timezone, all, encode, decode
                          , toString, fromString, timezoneOf
                          , default)

import Time
import TimeZone

import Dict
import Utils exposing (..)

import Json.Encode as JE
import Json.Decode exposing (Decoder)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

type Timezone = Timezone String (() -> Time.Zone)

default : Timezone
default = Timezone "Europe/Sofia" TimeZone.europe__sofia

all : List Timezone
all = TimeZone.zones
    |> Dict.toList
    |> List.map (\(name, f) -> Timezone name f)

encode : Timezone -> JE.Value
encode tz = JE.string (toString tz)

decode : Decoder Timezone
decode = JD.string
    |> JD.map fromString
    |> decodeOrFail "invalid timezone"

toString : Timezone -> String
toString (Timezone name _) = name

fromString : String -> Maybe Timezone
fromString name = Dict.get name TimeZone.zones
    |> Maybe.map (\f -> Timezone name f)

timezoneOf : Timezone -> Time.Zone
timezoneOf (Timezone _ tz) = tz ()
