module Calendar.TestDateTimeFormats exposing (..)

import Calendar exposing (..)
import Calendar.DateTimeFormats exposing (..)

import Time exposing (..)
import Random

import Parser

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Json
import Test exposing (..)
import Json.Decode as JD
import Json.Encode as JE

dateTests : Test
dateTests =
    describe "date format"
        [ Test.fuzz fuzzDateFormat "date parsing" testDateFormat
        , Test.fuzz fuzzTimeFormat "time parsing" testTimeFormat
        ]

testDateFormat : (String, (Int, Int, Int)) -> Expectation
testDateFormat (s, date) = Expect.equal
    ( Ok date )
    ( Parser.run dateParser s )

fuzzDateFormat : Fuzzer (String, (Int, Int, Int))
fuzzDateFormat = Fuzz.map (\((y, ys), (m, ms), (d, ds)) ->
    ((ys ++ "-" ++ ms ++ "-" ++ ds), (y, m, d)))
    fuzzDateFormatPaired

fuzzDateFormatPaired : Fuzzer ((Int, String), (Int, String), (Int, String))
fuzzDateFormatPaired = Fuzz.map3 (\x y z -> (x, y, z)) 
    (fuzzSInt 1900 3000)
    (fuzzSInt 1 12)
    (fuzzSInt 1 31)

testTimeFormat : (String, (Int, Int, Int)) -> Expectation
testTimeFormat (s, time) = Expect.equal
    ( Ok time )
    ( Parser.run timeParser s )

fuzzTimeFormat : Fuzzer (String, (Int, Int, Int))
fuzzTimeFormat = Fuzz.map (\((h, hs), (m, ms), (s, ss)) ->
    ((hs ++ ":" ++ ms ++ ":" ++ ss), (h, m, s)))
    fuzzTimeFormatPaired

fuzzTimeFormatPaired : Fuzzer ((Int, String), (Int, String), (Int, String))
fuzzTimeFormatPaired = Fuzz.map3 (\x y z -> (x, y, z)) 
    (fuzzSInt 0 23)
    (fuzzSInt 0 59)
    (fuzzSInt 0 60)

fuzzSInt : Int -> Int -> Fuzzer (Int, String)
fuzzSInt a b = Fuzz.oneOf
    [ Fuzz.intRange a b |> Fuzz.map (\i -> (i, String.fromInt i))
    , Fuzz.intRange a b |> Fuzz.map (\i -> (i, "0" ++ String.fromInt i))
    ]
