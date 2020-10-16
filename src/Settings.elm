module Settings exposing (..)

import Timezones as Timezones exposing (Timezone)

type alias Settings =
    { defaultTimezone : Timezone
    }

default : Settings
default =
    { defaultTimezone = Timezones.default
    }
