port module Storage exposing (..)

import Json.Encode as JE
import Json.Decode as JD

port gotCard    : (JE.Value -> msg) -> Sub msg
port missingCard  : (JE.Value -> msg) -> Sub msg
port getCard    : JE.Value -> Cmd msg
port saveCard   : JE.Value -> Cmd msg

port saveSettings : JE.Value -> Cmd msg

port attachFile             : () -> Cmd msg
port attachedFile           : (JE.Value -> msg) -> Sub msg
port downloadAttachedFile   : JE.Value -> Cmd msg

port exportData : () -> Cmd msg
port importData : () -> Cmd msg
port nukeData   : () -> Cmd msg

port syncNow    : () -> Cmd msg
port syncStatus : (JE.Value -> msg) -> Sub msg

port reload     : (JE.Value -> msg) -> Sub msg
