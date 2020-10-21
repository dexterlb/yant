module Calendar.DateTimeFormats exposing (dateParser, timeParser, parseDatePart, parseTimePart, timeToValue, dateToValue)

import Parser exposing ((|.), (|=), Parser)

import Calendar exposing (..)

dateToValue : DateTime -> String
dateToValue { year, month, day }
    =  (year |> String.fromInt |> String.padLeft 4 '0')
    ++ "-"
    ++ (monthToInt month |> String.fromInt |> String.padLeft 2 '0')
    ++ "-"
    ++ (day |> String.fromInt |> String.padLeft 2 '0')

timeToValue : DateTime -> String
timeToValue { hour, minute, second }
    =  (hour   |> String.fromInt |> String.padLeft 2 '0' )
    ++ ":"
    ++ (minute |> String.fromInt |> String.padLeft 2 '0' )
    ++ ":"
    ++ (second |> String.fromInt |> String.padLeft 2 '0' )

parseDatePart : String -> DateTime -> DateTime
parseDatePart s dt = case Parser.run dateParser s of
    Ok (year, monthN, day) -> case monthFromInt monthN of
        Just month -> { dt | year = year, month = month, day = day }
        Nothing    -> dt
    Err _ -> dt

parseTimePart : String -> DateTime -> DateTime
parseTimePart s dt = case Parser.run timeParser s of
    Ok (hour, minute, second) -> { dt | hour = hour, minute = minute, second = second }
    Err _ -> dt

timeParser : Parser (Int, Int, Int)
timeParser = Parser.succeed (\h m s -> (h, m, s))
    |= parsePrefixedInt
    |. Parser.symbol ":"
    |= parsePrefixedInt
    |. Parser.symbol ":"
    |= parsePrefixedInt

dateParser : Parser (Int, Int, Int)
dateParser = Parser.succeed (\y m d -> (y, m, d))
    |= parsePrefixedInt
    |. Parser.symbol "-"
    |= parsePrefixedInt
    |. Parser.symbol "-"
    |= parsePrefixedInt

parsePrefixedInt : Parser Int
parsePrefixedInt = 
    Parser.succeed String.toInt
    |= (Parser.getChompedString <| Parser.chompWhile Char.isDigit)
    |> Parser.andThen (\x -> case x of
        Just i -> Parser.succeed i
        Nothing -> Parser.problem "not a number" )
