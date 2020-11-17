module CommonComponents exposing (viewTZPicker)

import Html as HI exposing (Html)
import Html.Events as HE
import Html.Attributes as HA

import Calendar.Timezones as Timezones exposing (Timezone)

viewTZPicker : Timezone -> (Timezone -> msg) -> Html msg
viewTZPicker tz f =
    HI.select
        [ HA.class "timezone"
        , HE.onInput (\tzn -> case Timezones.fromString tzn of
            Just newTz -> f newTz
            Nothing    -> f tz)
        ]
        (List.map (tzOptionGroup tz) (Timezones.grouped Timezones.all))

tzOptionGroup : Timezone -> (String, List Timezone) -> Html msg
tzOptionGroup selectedTZ (groupName, tzs) =
    HI.optgroup
        [ HA.attribute "label" groupName ]
        (List.map (tzOption selectedTZ) tzs)

tzOption : Timezone -> Timezone -> Html msg
tzOption selectedTZ tz = let tzn = Timezones.toString tz in
    case tzn == Timezones.toString selectedTZ of
        True  -> HI.option [ HA.value tzn, HA.selected True  ] [ HI.text tzn ]
        False -> HI.option [ HA.value tzn, HA.selected False ] [ HI.text tzn ]

