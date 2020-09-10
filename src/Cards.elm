module Cards exposing
    (CardID, CardPath, Cards, noCards, Card
    )

import List.Nonempty as NE exposing (Nonempty)
import Dict as Dict exposing (Dict)

type alias CardID = String

type alias Card =
    { text:     String
    , children: List CardID
    }

type alias Cards = Dict CardID Card

noCards : Cards
noCards = Dict.empty

type alias CardPath = Nonempty CardID
