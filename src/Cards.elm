module Cards exposing
    ( CardID, CardPath, Cards, noCards, Card, add
    , encodeCardID, decodeCard, update
    )

import List.Nonempty as NE exposing (Nonempty)
import Dict as Dict exposing (Dict)
import Json.Encode as JE
import Json.Decode as JD

type alias CardID = String

type alias Card =
    { id:       CardID
    , text:     String
    , children: List CardID
    }

type alias Cards = Dict CardID Card

noCards : Cards
noCards = Dict.empty

add : Card -> Cards -> Cards
add card cards = Dict.insert card.id card cards

update : CardID -> (Card -> Card) -> Cards -> Cards
update id f cards = Dict.update id (Maybe.map f) cards

encodeCardID : CardID -> JE.Value
encodeCardID = JE.string

decodeCardID : JD.Decoder CardID
decodeCardID = JD.string

decodeCard : JD.Decoder Card
decodeCard = JD.map3 Card
    (JD.field "id" decodeCardID)
    (JD.field "text" JD.string)
    (JD.field "children" <| JD.list decodeCardID)

type alias CardPath = Nonempty CardID
