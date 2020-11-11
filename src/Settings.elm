module Settings exposing (..)

import Calendar.Timezones as Timezones exposing (Timezone)

type alias Settings =
    { defaultTimezone : Timezone
    , hideDone : Bool
    }

default : Settings
default =
    { defaultTimezone = Timezones.default
    , hideDone = False
    }
