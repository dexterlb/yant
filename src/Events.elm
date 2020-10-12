module Events exposing (..)

import Time
import Timezones exposing (Timezone)

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
