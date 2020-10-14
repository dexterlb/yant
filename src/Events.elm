module Events exposing (..)

import Time
import Time.Extra
import Timezones exposing (Timezone)

import Json.Encode as JE
import Json.Decode as JD

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

encodeDateTime : DateTime -> JE.Value
encodeDateTime dt
    = let fakePosixTime = Time.Extra.fromDateTuple Time.utc (dt.year, dt.month, dt.day)
                    |> Time.Extra.addHours dt.hour
                    |> Time.Extra.addMinutes dt.minute
                    |> Time.Extra.addSeconds dt.second
    in
        JE.object
            [ ( "date_time", JE.string (Time.Extra.toIso8601DateTimeUTC fakePosixTime) )
            , ( "timezone",  JE.string dt.timezone )
            ]

encodeFreq : Freq -> JE.Value
encodeFreq freq = JE.string (freqToString freq)

encodeWeekday : Weekday -> JE.Value
encodeWeekday freq = JE.string (weekdayToString freq)

encodeWeekStart : WeekStart -> JE.Value
encodeWeekStart freq = JE.string (weekStartToString freq)

encodeMonth : Month -> JE.Value
encodeMonth month = JE.int (monthToInt month)

encodeNoisiness : Noisiness -> JE.Value
encodeNoisiness n = JE.string (noisinessToString n)

freqToString : Freq -> String
freqToString freq = case freq of
    Secondly -> "SECONDLY"
    Minutely -> "MINUTELY"
    Daily    -> "DAILY"
    Weekly   -> "WEEKLY"
    Monthly  -> "MONTHLY"
    Yearly   -> "YEARLY"

weekdayToString : Weekday -> String
weekdayToString wd = case wd of
    Time.Mon -> "MO"
    Time.Tue -> "TU"
    Time.Wed -> "WE"
    Time.Thu -> "TH"
    Time.Fri -> "FR"
    Time.Sat -> "SA"
    Time.Sun -> "SU"

weekStartToString : WeekStart -> String
weekStartToString ws = case ws of
    Monday -> "MO"
    Sunday -> "SU"

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

noisinessToString : Noisiness -> String
noisinessToString n = case n of
    Noisy  -> "audio"
    Silent -> "display"

-- utils
catMaybes : List (Maybe a) -> List a
catMaybes l = case l of
    []               -> []
    ((Just x) :: xs) -> x :: (catMaybes xs)
    (Nothing  :: xs) ->       catMaybes xs

notEmpty : List a -> Maybe (List a)
notEmpty l = case l of
    [] -> Nothing
    _  -> Just l
