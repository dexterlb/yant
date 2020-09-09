module Cards exposing
    (Cards, noCards
    )

import Dict exposing (Dict)
import Dict

type alias CardID = String

type alias Card =
    { text:     String
    , children: List CardID
    }

type alias Cards = Dict CardID Card

noCards : Cards
noCards = Dict.empty
