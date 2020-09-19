module Utils exposing (hash, hash01)


import Bitwise
import Char
import String

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
