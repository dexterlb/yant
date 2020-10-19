module Utils exposing ( hash, hash01, catMaybes, notEmpty
                      , decodeOptional, decodeOptionalList, decodeOrFail
                      , onClick )


import Bitwise
import Char
import String

import Json.Encode as JE
import Json.Decode exposing (Decoder)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

import Html
import Html.Events as HE

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

onClick : msg -> Html.Attribute msg
onClick msg = HE.stopPropagationOn "click" (JD.succeed (msg, True))
