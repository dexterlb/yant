module TestEvents exposing (..)

import Events exposing (..)
import Timezones

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
        [ Fuzz.Json.roundtrip "EventData encode/decode" fuzzEventData encodeEventData decodeEventData ]

fuzzEventData : Fuzzer EventData
fuzzEventData = Fuzz.constant EventData
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
    |> fuzzField "timezone" (fuzzAny Timezones.allTimezones)

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

fuzzWeekday : Fuzzer Events.Weekday
fuzzWeekday = fuzzAny [ Mon , Tue , Wed , Thu , Fri , Sat , Sun ]

fuzzWeekStart : Fuzzer WeekStart
fuzzWeekStart = fuzzAny [ Monday, Sunday ]

fuzzMonth : Fuzzer Events.Month
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
