module TestEvents exposing (..)

import Events exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Json
import Test exposing (..)
import Json.Decode as JD
import Json.Encode as JE

fuzzEventData : Fuzzer EventData

jsonTests : Test
jsonTests =
    describe "json encoding and decoding"
        [ Fuzz.Json.roundtrip "EventData encode/decode" fuzzEventData encodeEventData decodeEventData ]
