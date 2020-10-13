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
    , until     : Termination
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

type Termination
    = Count Int
    | Until DateTime

type WeekStart
   = Monday
   | Sunday

type alias Duration = Int   -- number of seconds
type alias SignedDuration = Int


-- JSON stuff starts here

encodeEventData : EventData -> JE.Value
encodeEventData data = JE.object <| catMaybes
    [ Just ( "start", encodeDateTime data.start )
    , Maybe.map ( \end -> ( "end", encodeDateTime end ) data.end )
    , Just ( "allDay", JE.bool data.allDay )
    , Maybe.map ( \repeat -> ( "repeating", encodeRepeat repeat) ) data.repeat
    , Just ( "busy", JE.bool data.busy )
    , Just ( "isTask", JE.bool data.task )
    , Just ( "reminders", JE.list encodeReminder data.reminders )
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

-- utils
catMaybes : List (Maybe a) -> List a
catMaybes l = case l of
    []               -> []
    ((Just x) :: xs) -> x :: (catMaybes xs)
    (Nothing  :: xs) ->       catMaybes xs
