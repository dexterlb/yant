module Settings exposing (..)

import Calendar.Timezones as Timezones exposing (Timezone)

import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

type alias Settings =
    { defaultTimezone : Timezone
    , hideDone : Bool
    }

default : Settings
default =
    { defaultTimezone = Timezones.default
    , hideDone = False
    }

encode : Settings -> JE.Value
encode s = JE.object
    [ ( "default_timezone", Timezones.encode s.defaultTimezone )
    , ( "hide_done", JE.bool s.hideDone )
    ]
