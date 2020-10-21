module TestCalendar exposing (..)

import Calendar exposing (..)
import Calendar.Timezones as Timezones

import Time exposing (..)
import Random

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Json
import Test exposing (..)
import Json.Decode as JD
import Json.Encode as JE

jsonTests : Test
jsonTests =
    describe "json encoding and decoding"
        [ roundtrip "Event encode/decode" fuzzEvent encodeEvent decodeEvent
        , roundtrip "DateTime encode/decode" fuzzDateTime encodeDateTime decodeDateTime ]

fuzzEvent : Fuzzer Event
fuzzEvent = Fuzz.constant Event
    |> fuzzField   "start"  fuzzDateTime
    |> fuzzOptional "end"    fuzzDateTime
    |> fuzzField   "allDay" Fuzz.bool
    |> fuzzOptional "repeating" fuzzRepeat
    |> fuzzField   "busy"   Fuzz.bool
    |> fuzzField   "isTask" Fuzz.bool
    |> fuzzField   "alarms" (Fuzz.list fuzzReminder)

fuzzDateTime : Fuzzer DateTime
fuzzDateTime = Fuzz.constant DateTime
    |> fuzzField "year" (Fuzz.intRange 1900 3000)
    |> fuzzField "month" fuzzMonth
    |> fuzzField "day" (Fuzz.intRange 1 31) -- maybe 28 would be better? :)
    |> fuzzField "hour" (Fuzz.intRange 0 23)
    |> fuzzField "minute" (Fuzz.intRange 0 59)
    |> fuzzField "second" (Fuzz.intRange 0 60)
    |> fuzzField "timezone" (fuzzAny Timezones.all)

fuzzRepeat : Fuzzer Repeat
fuzzRepeat = Fuzz.constant Repeat
    |> fuzzField "freq" fuzzFreq
    |> fuzzField "interval" (Fuzz.intRange 1 Random.maxInt)
    |> fuzzField "filterSet" fuzzFilterSet
    |> fuzzField "wkst" fuzzWeekStart

fuzzReminder : Fuzzer Reminder
fuzzReminder = Fuzz.constant Reminder
    |> fuzzField "noisiness" fuzzNoisiness
    |> fuzzField "trigger" Fuzz.int
    |> fuzzField "repeat" fuzzReminderRepeat

fuzzFilterSet : Fuzzer FilterSet
fuzzFilterSet = Fuzz.constant FilterSet
    |> fuzzField "weekdays"    (Fuzz.list fuzzWeekday)
    |> fuzzField "monthdays"   (Fuzz.list <| Fuzz.intRange 1 31)
    |> fuzzField "months"      (Fuzz.list fuzzMonth)
    |> fuzzField "exclude"     (Fuzz.list fuzzDateTime)
    |> fuzzField "until"       (Fuzz.maybe fuzzDateTime)
    |> fuzzField "maxCount"    (Fuzz.maybe <| Fuzz.intRange 1 Random.maxInt)
    |> fuzzField "setPosition" (Fuzz.list Fuzz.int)

fuzzNoisiness : Fuzzer Noisiness
fuzzNoisiness = fuzzAny [Noisy, Silent]

fuzzWeekday : Fuzzer Calendar.Weekday
fuzzWeekday = fuzzAny [ Mon , Tue , Wed , Thu , Fri , Sat , Sun ]

fuzzWeekStart : Fuzzer WeekStart
fuzzWeekStart = fuzzAny [ Monday, Sunday ]

fuzzMonth : Fuzzer Calendar.Month
fuzzMonth = fuzzAny [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

fuzzReminderRepeat : Fuzzer ReminderRepeat
fuzzReminderRepeat = Fuzz.oneOf
    [ Fuzz.constant NoRepeat
    , Fuzz.map2 RepeatAt
        (Fuzz.intRange 1 Random.maxInt)
        (Fuzz.intRange 1 Random.maxInt)
    ]

fuzzFreq : Fuzzer Freq
fuzzFreq = fuzzAny [Secondly, Minutely, Hourly, Daily, Weekly, Monthly, Yearly]


fuzzField : String -> Fuzzer a -> Fuzzer (a -> b) -> Fuzzer b
fuzzField name a f = Fuzz.andMap a f

fuzzOptional : String -> Fuzzer a -> Fuzzer ((Maybe a) -> b) -> Fuzzer b
fuzzOptional name a f = fuzzField name (Fuzz.maybe a) f

fuzzAny : List a -> Fuzzer a
fuzzAny l = l |> List.map Fuzz.constant |> Fuzz.oneOf

{-| Roundtrip takes a fuzzer, a json encoder, a json decoder, and sees if an encode/decode cycle succeeds without losing any data.
-}
roundtrip : String -> Fuzzer a -> (a -> JE.Value) -> JD.Decoder a -> Test
roundtrip name fuzz encode decode =
    Test.fuzz fuzz ("json roundtrip tests for " ++ name) <|
        \a -> (let val = a |> encode |> JE.encode 0
                in (val, val |> JD.decodeString decode) |> Expect.equal (val, Ok a) )
