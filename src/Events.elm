module Events exposing (..)

import Time
import Time.Extra
import Timezones exposing (..)

import Utils exposing (..)

import Json.Encode as JE
import Json.Decode exposing (Decoder)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

type alias Event =
    { data        : EventData
    , summary     : String
    , description : String
    , timestamp   : Maybe DateTime
    , status      : EventStatus
    }

type alias EventData =
    { start  : DateTime
    , end    : Maybe DateTime
    , allDay : Bool
    , repeat : Maybe Repeat
    , busy   : Bool         -- treat the human associated with the event as busy
    , task   : Bool         -- treat the event as a to-do item
    , reminders : List Reminder
    }

type alias Repeat =
    { freq      : Freq
    , interval  : Int       -- every <interval> <freq>'s
    , filterSet : FilterSet
    , wkst      : WeekStart -- this matters because there are time zones
                            -- containing cities with different week starts
                            -- so events with a timezone different from
                            -- the one being displayed need to take this
                            -- into account. Welcome to hell.
    }

type alias FilterSet =
    { weekdays    : List Weekday
    , monthdays   : List Monthday
    , months      : List Month
    , exclude     : List DateTime
    , until       : Maybe DateTime
    , maxCount    : Maybe Int
    , setPosition : List Int    -- after determining all occurences matching
                                -- the above filters, only take those whose
                                -- indices are in this list.
                                -- used for shit like "the last thursday of november"
    }

type alias Reminder =
    { noisiness : Noisiness
    , trigger   : SignedDuration    -- before or after the event
    , repeat    : ReminderRepeat
    }

type ReminderRepeat
    = NoRepeat
    | RepeatAt Int Duration           -- how many times, at what interval

type Noisiness
    = Noisy
    | Silent

type Freq
    = Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly

type EventStatus
    = Done
    | NotDone   -- actually more stuff here, but might not need it

type alias DateTime =
    { year : Year
    , month : Month
    , day   : Monthday
    , hour  : Hour
    , minute : Minute
    , second : Second
    , timezone : Timezone
    }

type alias Year     = Int
type alias Monthday = Int   -- usually between 1 and 28, but sometimes up to 31 :)
type alias Weekday  = Time.Weekday
type alias Month    = Time.Month
type alias Hour     = Int
type alias Minute   = Int
type alias Second   = Int

type EventDuration
    = Instant
    | Continous Duration

type WeekStart
   = Monday
   | Sunday

type alias Duration = Int   -- number of seconds
type alias SignedDuration = Int

-- *******

-- defaultEventData : Timezone -> Time.Posix -> EventData
-- defaultEventData tz 

-- JSON stuff starts here

encodeEventData : EventData -> JE.Value
encodeEventData data = JE.object <| catMaybes
    [ Just ( "start", encodeDateTime data.start )
    , Maybe.map ( \end -> ( "end", encodeDateTime end ) ) data.end
    , Just ( "allDay", JE.bool data.allDay )
    , Maybe.map ( \repeat -> ( "repeating", encodeRepeat repeat) ) data.repeat
    , Just ( "busy", JE.bool data.busy )
    , Just ( "isTask", JE.bool data.task )
    , Just ( "alarms", JE.list encodeReminder data.reminders )
    ]

decodeEventData : Decoder EventData
decodeEventData = JD.succeed EventData
    |> JDP.required   "start"  decodeDateTime
    |> decodeOptional "end"    decodeDateTime
    |> JDP.optional   "allDay" JD.bool False
    |> decodeOptional "repeating" decodeRepeat
    |> JDP.optional   "busy"   JD.bool False
    |> JDP.required   "isTask" JD.bool
    |> JDP.optional   "alarms" (JD.list decodeReminder) []

encodeRepeat : Repeat -> JE.Value
encodeRepeat rep = JE.object <| catMaybes
    [ Just ( "freq", encodeFreq rep.freq )
    , Just ( "interval", JE.int rep.interval )
    , Maybe.map ( \x -> ( "until", encodeDateTime x ) )
                rep.filterSet.until
    , Maybe.map ( \x -> ( "count", JE.int x ) )
                rep.filterSet.maxCount
    , Maybe.map ( \xs -> ( "byDay", JE.list encodeWeekday xs ) )
                ( notEmpty rep.filterSet.weekdays )
    , Maybe.map ( \xs -> ( "byMonth", JE.list encodeMonth xs ) )
                ( notEmpty rep.filterSet.months )
    , Maybe.map ( \xs -> ( "byMonthDay", JE.list JE.int xs ) )
                ( notEmpty rep.filterSet.monthdays )
    , Maybe.map ( \xs -> ( "exclude", JE.list encodeDateTime xs ) )
                ( notEmpty rep.filterSet.exclude )
    , Maybe.map ( \xs -> ( "bySetPos", JE.list JE.int xs ) )
                ( notEmpty rep.filterSet.setPosition )
    , Just ( "wkst", encodeWeekStart rep.wkst )
    ]

decodeRepeat : Decoder Repeat
decodeRepeat = JD.map4 Repeat
    ( JD.field "freq" decodeFreq)
    ( JD.oneOf [ ( JD.field "interval" JD.int ), JD.succeed 1 ] )
    decodeFilterSet
    ( JD.oneOf [ ( JD.field "wkst" decodeWeekStart ), JD.succeed Monday ] )

decodeFilterSet : Decoder FilterSet
decodeFilterSet = JD.succeed FilterSet
    |> decodeOptionalList "byDay" decodeWeekday
    |> decodeOptionalList "byMonthDay" JD.int
    |> decodeOptionalList "byMonth" decodeMonth
    |> decodeOptionalList "exclude" decodeDateTime
    |> decodeOptional     "until" decodeDateTime
    |> decodeOptional     "count" JD.int
    |> decodeOptionalList "bySetPos" JD.int


encodeReminder : Reminder -> JE.Value
encodeReminder rem = JE.object <| catMaybes
    [ Just ( "type", encodeNoisiness rem.noisiness )
    , Just ( "trigger", JE.int rem.trigger )
    , case rem.repeat of
        NoRepeat -> Nothing
        RepeatAt n _ -> Just ( "repeat", JE.int n )
    , case rem.repeat of
        NoRepeat -> Nothing
        RepeatAt _ int -> Just ( "interval", JE.int int )
    ]

decodeReminder : Decoder Reminder
decodeReminder = JD.map3 Reminder
    ( JD.field "type" decodeNoisiness )
    ( JD.field "trigger" JD.int )
    ( JD.oneOf [ decodeReminderRepeat, JD.succeed NoRepeat ] )

decodeReminderRepeat : Decoder ReminderRepeat
decodeReminderRepeat =
    JD.map2 RepeatAt
        ( JD.field "repeat"   JD.int )
        ( JD.field "interval" JD.int )

encodeDateTime : DateTime -> JE.Value
encodeDateTime dt =
    JE.object
        [ ( "year",      JE.int         dt.year )
        , ( "month",     JE.int         (monthToInt dt.month) )
        , ( "day",       JE.int         dt.day )
        , ( "hour",      JE.int         dt.hour )
        , ( "minute",    JE.int         dt.minute )
        , ( "second",    JE.int         dt.second )
        , ( "timezone",  encodeTimezone dt.timezone  )
        ]

decodeDateTime : Decoder DateTime
decodeDateTime = JD.succeed DateTime
    |> JDP.required "year"     JD.int
    |> JDP.required "month"    (JD.map monthFromInt JD.int |> decodeOrFail "not a valid month")
    |> JDP.required "day"      JD.int
    |> JDP.required "hour"     JD.int
    |> JDP.required "minute"   JD.int
    |> JDP.required "second"   JD.int
    |> JDP.required "timezone" decodeTimezone

encodeFreq : Freq -> JE.Value
encodeFreq freq = JE.string (freqToString freq)

decodeFreq : Decoder Freq
decodeFreq = JD.string
    |> JD.map freqFromString
    |> decodeOrFail "not a valid Freq"

encodeWeekday : Weekday -> JE.Value
encodeWeekday freq = JE.string (weekdayToString freq)

decodeWeekday : Decoder Weekday
decodeWeekday = JD.string
    |> JD.map weekdayFromString
    |> decodeOrFail "not a valid weekday"

encodeWeekStart : WeekStart -> JE.Value
encodeWeekStart freq = JE.string (weekStartToString freq)

decodeWeekStart : Decoder WeekStart
decodeWeekStart = JD.string
    |> JD.map weekStartFromString
    |> decodeOrFail "not a valid week start"

encodeMonth : Month -> JE.Value
encodeMonth month = JE.int (monthToInt month)

decodeMonth : Decoder Month
decodeMonth = JD.int
    |> JD.map monthFromInt
    |> decodeOrFail "not a valid month"

encodeNoisiness : Noisiness -> JE.Value
encodeNoisiness n = JE.string (noisinessToString n)

decodeNoisiness : Decoder Noisiness
decodeNoisiness = JD.string
    |> JD.map noisinessFromString
    |> decodeOrFail "not a valid noisiness"


freqToString : Freq -> String
freqToString freq = case freq of
    Secondly -> "SECONDLY"
    Minutely -> "MINUTELY"
    Hourly   -> "HOURLY"
    Daily    -> "DAILY"
    Weekly   -> "WEEKLY"
    Monthly  -> "MONTHLY"
    Yearly   -> "YEARLY"

freqFromString : String -> Maybe Freq
freqFromString s = case s of
    "SECONDLY" -> Just Secondly
    "MINUTELY" -> Just Minutely
    "HOURLY"   -> Just Hourly
    "DAILY"    -> Just Daily
    "WEEKLY"   -> Just Weekly
    "MONTHLY"  -> Just Monthly
    "YEARLY"   -> Just Yearly
    _          -> Nothing

weekdayToString : Weekday -> String
weekdayToString wd = case wd of
    Time.Mon -> "MO"
    Time.Tue -> "TU"
    Time.Wed -> "WE"
    Time.Thu -> "TH"
    Time.Fri -> "FR"
    Time.Sat -> "SA"
    Time.Sun -> "SU"

weekdayFromString : String -> Maybe Weekday
weekdayFromString s = case s of
    "MO" -> Just Time.Mon
    "TU" -> Just Time.Tue
    "WE" -> Just Time.Wed
    "TH" -> Just Time.Thu
    "FR" -> Just Time.Fri
    "SA" -> Just Time.Sat
    "SU" -> Just Time.Sun
    _    -> Nothing

weekStartToString : WeekStart -> String
weekStartToString ws = case ws of
    Monday -> "MO"
    Sunday -> "SU"

weekStartFromString : String -> Maybe WeekStart
weekStartFromString s = case s of
    "MO" -> Just Monday
    "SU" -> Just Sunday
    _    -> Nothing

monthToInt : Month -> Int
monthToInt month = case month of
    Time.Jan ->  1
    Time.Feb ->  2
    Time.Mar ->  3
    Time.Apr ->  4
    Time.May ->  5
    Time.Jun ->  6
    Time.Jul ->  7
    Time.Aug ->  8
    Time.Sep ->  9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12

monthFromInt : Int -> Maybe Month
monthFromInt i = case i of
    1  -> Just Time.Jan
    2  -> Just Time.Feb
    3  -> Just Time.Mar
    4  -> Just Time.Apr
    5  -> Just Time.May
    6  -> Just Time.Jun
    7  -> Just Time.Jul
    8  -> Just Time.Aug
    9  -> Just Time.Sep
    10 -> Just Time.Oct
    11 -> Just Time.Nov
    12 -> Just Time.Dec
    _  -> Nothing

noisinessToString : Noisiness -> String
noisinessToString n = case n of
    Noisy  -> "audio"
    Silent -> "display"

noisinessFromString : String -> Maybe Noisiness
noisinessFromString s = case s of
    "audio"   -> Just Noisy
    "display" -> Just Silent
    _         -> Nothing
