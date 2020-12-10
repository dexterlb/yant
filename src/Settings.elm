module Settings exposing (..)

import Calendar.Timezones as Timezones exposing (Timezone)

import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

type alias Settings =
    { defaultTimezone : Timezone
    , hideDone : Bool
    , sync: SyncSettings
    }

type alias SyncSettings =
    { server: String
    , space: String
    , secret: String
    , enabled: Bool
    }

default : Settings
default =
    { defaultTimezone = Timezones.default
    , hideDone = False
    , sync = defaultSyncSettings
    }

encode : Settings -> JE.Value
encode s = JE.object
    [ ( "default_timezone", Timezones.encode   s.defaultTimezone )
    , ( "hide_done",        JE.bool            s.hideDone )
    , ( "sync",             encodeSyncSettings s.sync )
    ]

decode : JD.Decoder Settings
decode = JD.succeed Settings
    |> JDP.optional "default_timezone" Timezones.decode     Timezones.default
    |> JDP.optional "hide_done"        JD.bool              False
    |> JDP.optional "sync"             decodeSyncSettings   defaultSyncSettings

decodeSyncSettings : JD.Decoder SyncSettings
decodeSyncSettings = JD.succeed SyncSettings
    |> JDP.required "server" JD.string
    |> JDP.required "space" JD.string
    |> JDP.required "secret" JD.string
    |> JDP.required "enabled" JD.bool

encodeSyncSettings : SyncSettings -> JE.Value
encodeSyncSettings s = JE.object
    [ ( "server",  JE.string s.server  )
    , ( "space",   JE.string s.space   )
    , ( "secret",  JE.string s.secret  )
    , ( "enabled", JE.bool   s.enabled )
    ]

defaultSyncSettings : SyncSettings
defaultSyncSettings =
    { server  = "https://example.com/ass"
    , space   = "foo"
    , secret  = "bar"
    , enabled = False
    }
