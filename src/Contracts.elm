module Contracts exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, andThen, bool, decodeString, dict, fail, field, float, int, null, oneOf, string, succeed, list, value)
import Json.Encode as JE
import Result
import Set


type alias Data =
    Dict String JE.Value


type alias Type = { t: TypeDescr, meta: Data }

type TypeDescr
    = TVoid
    | TNil
    | TInt
    | TFloat
    | TString
    | TBool
    | TLiteral JE.Value
    | TUnion (List Type)
    | TMap Type Type
    | TList Type
    | TTuple (List Type)
    | TStruct (Dict String Type)
    | TUnknown String


type Contract
    = Constant Value Children
    | MapContract Children
    | Function Callee Children
    | PropertyKey Property Children

type alias Children = Dict String Contract


-- need better names for those

type alias Topic = String

type alias Callee =
    { argument : Type
    , path : Topic
    , retval : Type
    }


type alias PropertyID =
    Topic


type alias ContractProperties =
    Dict PropertyID Value


type alias Property =
    { setter : Maybe Callee
    , propertyType : Type
    , path : PropertyID
    }


type Value
    = SimpleInt Int
    | SimpleString String
    | SimpleFloat Float
    | SimpleBool Bool
    | Complex JE.Value
    | Loading

valueDecoder : Decoder Value
valueDecoder = JD.oneOf
    [ JD.map SimpleInt    JD.int
    , JD.map SimpleString JD.string
    , JD.map SimpleBool   JD.bool
    , JD.map SimpleFloat  JD.float
    , JD.map Complex      JD.value
    ]

parseValue : JE.Value -> Value
parseValue v = JD.decodeValue valueDecoder v |> Result.withDefault (Complex v)

valueEncoder : Value -> JE.Value
valueEncoder v = case v of
    SimpleInt    x -> JE.int x
    SimpleString x -> JE.string x
    SimpleFloat  x -> JE.float x
    SimpleBool   x -> JE.bool x
    Complex      x -> x
    Loading        -> JE.null

encodeValue : Value -> Maybe JE.Value
encodeValue v = case v of
    Loading -> Nothing
    x       -> Just <| valueEncoder x

equivTypes : Type -> Type -> Bool
equivTypes a b =
    a.t == b.t  -- FIXME


parseContract : String -> Result String Contract
parseContract s =
    decodeString contractDecoder s
        |> Result.mapError JD.errorToString


contractDecoder : Decoder Contract
contractDecoder =
    oneOf
        [ objectDecoder
        , JD.lazy (\_ -> mapContractDecoder)
        ]


objectDecoder : Decoder Contract
objectDecoder =
    field "_t" string
        |> andThen
            (\t ->
                case t of
                    "value" ->
                        propertyDecoder

                    "callable" ->
                        functionDecoder

                    "constant" ->
                        constantDecoder

                    _ ->
                        fail <| "object type `" ++ t ++ "' is unknown"
            )


propertyDecoder : Decoder Contract
propertyDecoder =
    JD.map2 makeProperty
        (field "type" schemaDecoder)
        (field "subcontract" mapDecoder)

functionDecoder : Decoder Contract
functionDecoder =
    JD.map3 makeFunction
        (field "argument" schemaDecoder)
        (field "retval" schemaDecoder)
        (field "subcontract" mapDecoder)

constantDecoder : Decoder Contract
constantDecoder =
    JD.map2 makeConstant
        (field "value" valueDecoder)
        (field "subcontract" mapDecoder)


mapContractDecoder : Decoder Contract
mapContractDecoder = JD.map MapContract mapDecoder

mapDecoder : Decoder Children
mapDecoder = dict (JD.lazy (\_ -> contractDecoder))


dataDecoder : Decoder Data
dataDecoder =
    dict JD.value


makeProperty : Type -> Children -> Contract
makeProperty t subcontract = PropertyKey
    { path = "", propertyType = t, setter = Nothing }
    subcontract

makeFunction : Type -> Type -> Children -> Contract
makeFunction argument retval subcontract =
    Function
        { argument = argument
        , path = ""
        , retval = retval
        }
        subcontract

makeConstant : Value -> Children -> Contract
makeConstant value subcontract =
    Constant
        value
        subcontract


dataEncoder : Data -> JE.Value
dataEncoder d =
    JE.object (Dict.toList d)


schemaDecoder : Decoder Type
schemaDecoder = JD.field "t" typeDecoder

typeDecoder : Decoder Type
typeDecoder =
    (oneOf
        [ JD.field "meta" dataDecoder
        , JD.succeed Dict.empty ]
    ) |> andThen
        (\meta ->
            JD.map (\t ->
                { meta = meta, t = t }
            ) typeDescrDecoder
        )

recursiveTypeDecoder : Decoder Type
recursiveTypeDecoder = JD.lazy <| \_ -> typeDecoder

typeDescrDecoder : Decoder TypeDescr
typeDescrDecoder =
    oneOf
        [ recursiveTypeDescrDecoder
        , tUnknownDecoder
        ]


recursiveTypeDescrDecoder : Decoder TypeDescr
recursiveTypeDescrDecoder =
    JD.lazy <|
        \_ ->
            oneOf
                [ tBasicDecoder
                , JD.lazy <| \_ -> tComplexDecoder
                ]


tNilDecoder : Decoder TypeDescr
tNilDecoder =
    null TNil


tBasicDecoder : Decoder TypeDescr
tBasicDecoder =
    string
        |> andThen
            (\name ->
                case name of
                    "void" ->
                        succeed TVoid

                    "null" ->
                        succeed TNil

                    "int" ->
                        succeed TInt

                    "bool" ->
                        succeed TBool

                    "float" ->
                        succeed TFloat

                    "string" ->
                        succeed TString

                    _ ->
                        fail <| "type '" ++ name ++ "' is not a basic type"
            )


tComplexDecoder : Decoder TypeDescr
tComplexDecoder =
    JD.field "kind" string
        |> andThen
            (\t ->
                case t of
                    "type-basic" ->
                        JD.field "sub" tBasicDecoder

                    "type-literal" ->
                        tLiteralDecoder

                    "type-map" ->
                        tMapDecoder

                    "type-union" ->
                        tUnionDecoder

                    "type-list" ->
                        tListDecoder

                    "type-tuple" ->
                        tTupleDecoder

                    "type-struct" ->
                        tStructDecoder

                    _ ->
                        fail <| "complex type '" ++ t ++ "' is unknown"
            )


tStructDecoder : Decoder TypeDescr
tStructDecoder =
    JD.field "fields" <|
        JD.map TStruct <|
            JD.dict recursiveTypeDecoder


tTupleDecoder : Decoder TypeDescr
tTupleDecoder =
    JD.field "fields" <|
        JD.map TTuple <|
            JD.list recursiveTypeDecoder


tListDecoder : Decoder TypeDescr
tListDecoder =
    JD.map TList
        (JD.field "value" recursiveTypeDecoder)


tMapDecoder : Decoder TypeDescr
tMapDecoder =
    JD.map2 TMap
        (JD.field "key" recursiveTypeDecoder)
        (JD.field "value" recursiveTypeDecoder)


tLiteralDecoder : Decoder TypeDescr
tLiteralDecoder =
    JD.map TLiteral
        (JD.field "value" <| JD.value
        )

tUnionDecoder : Decoder TypeDescr
tUnionDecoder =
    JD.map TUnion
        (JD.field "alts" <| JD.list
        <| recursiveTypeDecoder)


tUnknownDecoder : Decoder TypeDescr
tUnknownDecoder =
    JD.map
        (\v -> TUnknown <| JE.encode 4 v)
        JD.value


inspectType : Type -> String
inspectType { t, meta } = inspectTypeDescr t ++ inspectData meta

inspectTypeDescr : TypeDescr -> String
inspectTypeDescr givenType =
    case givenType of
        TStruct d ->
            d
                |> Dict.toList
                |> List.map (\( k, v ) -> k ++ ": " ++ inspectType v)
                |> String.join ", "
                |> (\s -> "{ " ++ s ++ " }")

        TTuple l ->
            l
                |> List.map inspectType
                |> String.join ", "
                |> (\s -> "( " ++ s ++ " )")

        TUnion l ->
            "(" ++ (String.join " | " (List.map inspectType l)) ++ ")"

        TLiteral json ->
            JE.encode 0 json

        TList t ->
            "list[" ++ inspectType t ++ "]"

        TMap k v ->
            "map[" ++ inspectType k ++ " → " ++ inspectType v ++ "]"

        TInt ->
            "int"

        TFloat ->
            "float"

        TString ->
            "string"

        TBool ->
            "bool"

        TNil ->
            "nil"

        TVoid ->
            "void"

        TUnknown v ->
            v


inspectData : Data -> String
inspectData d = if Dict.isEmpty d then
        ""
    else
        d
            |> Dict.toList
            |> List.map (\( k, v ) -> k ++ ": " ++ JE.encode 0 v)
            |> String.join ", "
            |> (\s -> "<" ++ s ++ ">")


propertify : Contract -> ContractProperties -> ( Contract, ContractProperties )
propertify contract prevProperties =
    let
        ( newContract, properties ) =
            propertify_ "" contract prevProperties ( Dict.fromList [] )
    in
        ( newContract, properties )


propertify_ : Topic -> Contract -> ContractProperties -> ContractProperties -> ( Contract, ContractProperties )
propertify_ path contract prevProperties properties =
    case contract of
        MapContract d ->
            let
                ( subcontractList, newProperties ) =
                    propertifyMap path (Dict.toList d) prevProperties properties

                subcontract =
                    MapContract <| Dict.fromList subcontractList
            in
                ( subcontract, newProperties )

        Function f subcontract ->
            let
                ( subcontractList, newProperties ) =
                    propertifyMap path (Dict.toList subcontract) prevProperties properties
            in
                ( Function { f | path = path } (Dict.fromList subcontractList), newProperties )


        PropertyKey prop subcontract ->
            let
                ( subcontractList, newProperties ) =
                    propertifyMap path (Dict.toList subcontract) prevProperties properties
                newerProperties =
                    Dict.insert path (Maybe.withDefault Loading (Dict.get path prevProperties)) newProperties
            in let
                subcontractDict = Dict.fromList subcontractList
            in
                ( PropertyKey
                    { prop | path = path, setter = makeSetter prop subcontractDict }
                    subcontractDict
                , newerProperties )

        Constant c subcontract ->
            let
                ( subcontractList, newProperties ) =
                    propertifyMap path (Dict.toList subcontract) prevProperties properties
            in
                ( Constant c (Dict.fromList subcontractList), newProperties )



makeSetter : Property -> Children -> Maybe Callee
makeSetter { propertyType } children =
    Dict.get "set" children |> Maybe.andThen getFunction
        |> Maybe.andThen (checkSetterType propertyType)


checkSetterType : Type -> Callee -> Maybe Callee
checkSetterType t f = case equivTypes f.argument t of
    True  -> Just f
    False -> Nothing

numericContract : Contract -> Maybe Float
numericContract c =
    case c of
        Constant (SimpleFloat f) _ ->
            Just f

        Constant (SimpleInt i) _ ->
            Just <| toFloat i

        _ ->
            Nothing


getFunction : Contract -> Maybe Callee
getFunction c =
    case c of
        Function f _ ->
            Just f

        _ ->
            Nothing


propertifyMap : Topic -> List ( String, Contract ) -> ContractProperties -> ContractProperties -> ( List ( String, Contract ), ContractProperties )
propertifyMap path l prevProperties data =
    case l of
        [] ->
            ( [], data )

        ( hk, hv ) :: t ->
            let
                ( newTail, newData1 ) =
                    propertifyMap path t prevProperties data
            in
            let
                ( contract, newData2 ) =
                    propertify_ (appendPath path hk) hv prevProperties newData1
            in
            ( ( hk, contract ) :: newTail, newData2 )


-- todo: make those work on deep types

getTypeFields : Type -> Dict String JE.Value
getTypeFields { meta } = meta



-- utils

appendPath : Topic -> Topic -> Topic
appendPath a b = case (a, b) of
    ("", "") -> ""
    (_, "")  -> a
    ("", _)  -> b
    _        -> a ++ "/" ++ b

fetch : comparable -> Dict comparable v -> v
fetch k d =
    case Dict.get k d of
        Just v ->
            v

            -- here be dragons
        Nothing -> makeUndefined k

-- warning: you will feel dirty and violated after grasping the following function:
makeUndefined : a -> b
makeUndefined a = (\_ -> makeUndefined a) ()


firstJust : List (Maybe a) -> Maybe a
firstJust l =
    case l of
        [] ->
            Nothing

        (Just x) :: _ ->
            Just x

        Nothing :: t ->
            firstJust t


emptyData : Data
emptyData =
    Dict.empty

type TypeError
    = NoError
    | CannotCoerce JE.Value Type
    | NotSupported String
    | KeysDiffer (List String) (List String)
    | WrongLiteral JE.Value JE.Value

typeErrorToString : TypeError -> String
typeErrorToString err = case err of
    NoError          -> "no error"
    CannotCoerce v t -> "cannot coerce '" ++ (JE.encode 0 v) ++ "' to type " ++ (inspectType t)
    NotSupported s   -> s
    KeysDiffer a b   -> "keys differ: " ++ (String.join "," a) ++ " ≠ " ++ (String.join "," b)
    WrongLiteral a b -> "value '" ++ (JE.encode 0 b) ++ "' does not match literal '" ++ (JE.encode 0 a) ++ "' ??"

typeCheck : Type -> JE.Value -> TypeError
typeCheck t v = JD.decodeValue (typeChecker t) v
    |> Result.withDefault (CannotCoerce v t)

-- oops, this is semantically wrong
typeChecker : Type -> Decoder TypeError
typeChecker typ =  -- todo: use the metadata
    let
        ok = andThen (\_ -> succeed NoError)
        reduce = andThen (succeed << (List.foldl typeErrorPlus NoError))
        { t } = typ
    in case t of
        TNil            -> null     NoError
        TInt            -> int      |> ok
        TFloat          -> float    |> ok
        TString         -> string   |> ok
        TBool           -> bool     |> ok
        TLiteral ev     -> JD.andThen (\v -> case v == ev of
            True  -> succeed NoError
            False -> fail <| typeErrorToString <| WrongLiteral ev v) value
        TUnion l        -> oneOf    <| List.map typeChecker l
        TList t_        -> list (typeChecker t_) |> reduce
        TMap keyType t_ -> case keyType.t of
            TString     -> dict (typeChecker t_) |> andThen (succeed << Dict.values) |> reduce
            _           -> succeed  <| NotSupported "maps with non-string keys not supported yet"
        TTuple _        -> succeed  <| NotSupported "tuples not supported yet"
        TStruct d       -> dict value |> andThen (structChecker d)
        _               -> value    |> andThen (\v -> succeed <| CannotCoerce v typ)

structChecker : Dict String Type -> Dict String JE.Value -> Decoder TypeError
structChecker td vd = succeed <| case sameKeys td vd of
    False -> KeysDiffer (Dict.keys td) (Dict.keys vd)
    True  -> Dict.toList td
        |> List.map (\(k, t) -> typeCheck t (fetch k vd))
        |> List.foldl typeErrorPlus NoError

typeErrorPlus : TypeError -> TypeError -> TypeError
typeErrorPlus e1 e2 = case (e1, e2) of
    (NoError, NoError) -> NoError
    (NoError, _      ) -> e2
    (_      , _      ) -> e1

sameKeys : Dict comparable v1 -> Dict comparable v2 -> Bool
sameKeys a b = (a |> Dict.keys |> Set.fromList) == (b |> Dict.keys |> Set.fromList)
