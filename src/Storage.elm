port module Storage exposing (..)

import Json.Encode as JE
import Json.Decode as JD

port gotCard    : (JE.Value -> msg) -> Sub msg
port getCard    : JE.Value -> Cmd msg
port saveCard   : JE.Value -> Cmd msg
