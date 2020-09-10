module PathTree exposing (PathTree, put, member, empty)

import Cards exposing (CardID, CardPath)
import Dict as Dict exposing (Dict)
import List.Nonempty as NE

type alias PathTree a = Dict (List CardID) a

put : CardPath -> a -> PathTree a -> PathTree a
put p v t = Dict.insert (NE.toList p) v t

member : CardPath -> PathTree a -> Bool
member p t = Dict.member (NE.toList p) t

empty : PathTree a
empty = Dict.empty
