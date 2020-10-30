module Utils exposing ( hash, hash01, catMaybes, notEmpty
                      , decodeOptional, decodeOptionalList, decodeOrFail
                      , onClick, checkbox, indicator
                      , silentDelete, silentUpdate, setMember
                      , whenEmpty, emptyWhenHasAll )


import Bitwise
import Char
import String

import Json.Encode as JE
import Json.Decode exposing (Decoder)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

import Html as HT exposing (Html)
import Html.Events as HE
import Html.Attributes as HA

-- hashing utils

hash01 : String -> Float
hash01 s = modBy 1024 (hash s)
        |> toFloat
        |> (\x -> x / 1024.0)

{-|
Hashes a String to an Int using the
[djb2](http://www.cse.yorku.ca/~oz/hash.html) algorithm. This is in no way
cryptographically secure. It is just for turning abirary strings in to numbers.
Original version from [jergason/elm-hash](https://github.com/jergason/elm-hash).
    hash "yolo swaggins" == 2438413579
-}
hash : String -> Int
hash str = String.foldl updateHash 5381 str


updateHash : Char -> Int -> Int
updateHash c h =
  (Bitwise.shiftLeftBy 5 h) + h + Char.toCode c


-- data structure utils

catMaybes : List (Maybe a) -> List a
catMaybes l = case l of
    []               -> []
    ((Just x) :: xs) -> x :: (catMaybes xs)
    (Nothing  :: xs) ->       catMaybes xs

notEmpty : List a -> Maybe (List a)
notEmpty l = case l of
    [] -> Nothing
    _  -> Just l

silentDelete : Int -> List a -> List a
silentDelete idx l = case (idx, l) of
    (0, (_ :: xs)) -> xs
    (_, [])        -> []
    (_, (x :: xs)) -> x :: (silentDelete (idx - 1) xs)

silentUpdate : Int -> a -> List a -> List a
silentUpdate idx elem l = case (idx, l) of
    (0, (_ :: xs)) -> elem :: xs
    (_, [])        -> []
    (_, (x :: xs)) -> x :: (silentUpdate (idx - 1) elem xs)

setMember : a -> Bool -> List a -> List a
setMember x b l = case (l, b) of
    ([], True) -> [x]
    ([], False) -> []
    ((y :: xs), False) -> case x == y of
        True -> setMember x False xs
        False -> y :: (setMember x False xs)
    ((y :: xs), True) -> case x == y of
        True -> y :: (setMember x False xs)
        False -> y :: (setMember x True xs)

whenEmpty : List a -> List a -> List a
whenEmpty default l = case l of
    [] -> default
    _  -> l

emptyWhenHasAll : List a -> List a -> List a
emptyWhenHasAll items l =
    case List.all (\x -> List.member x l) items of
        True  -> []
        False -> l


-- json utils

decodeOptional : String -> Decoder a -> Decoder ((Maybe a) -> b) -> Decoder b
decodeOptional key dec pipe = JDP.optional key (JD.map Just dec) Nothing pipe

decodeOptionalList : String -> Decoder a -> Decoder ((List a) -> b) -> Decoder b
decodeOptionalList key dec pipe = JDP.optional key (JD.list dec) [] pipe

decodeOrFail : String -> Decoder (Maybe a) -> Decoder a
decodeOrFail msg d = d |> JD.andThen (\m -> case m of
    Nothing -> JD.fail msg
    Just  x -> JD.succeed x)

-- html utils

onClick : msg -> HT.Attribute msg
onClick msg = HE.stopPropagationOn "click" (JD.succeed (msg, True))

checkbox : List (HT.Attribute msg) -> List (Html msg) -> Html msg
checkbox attrs labelElements = HT.span []
    [ HT.input
      ( (HA.type_ "checkbox") :: attrs )
      []
    , HT.label
      []
      labelElements
    ]

indicator : String -> String -> Html msg
indicator className textContent =
    HT.div
        [ HA.class "indicator", HA.class className, HA.title textContent ]
        [ HT.span [ HA.class "sr-only" ] [ HT.text textContent ] ]

