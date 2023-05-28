module Calendar exposing (..)

import Date exposing (..)
import DateTypes exposing (DateTime)
import List.Extra as List

-- strftime

type AmPm
    = AM
    | PM

type alias FormatOptions =
    { preferredDate : String
    , preferredTime : String
    , preferredDateTime : String
    , amPmNames : AmPm -> String
    , monthNames : Int -> Maybe String
    , dayOfWeekNames : Int -> Maybe String
    , abbreviatedMonthNames : Int -> Maybe String
    , abbreviatedDayOfWeekNames : Int -> Maybe String
    , preferredDateTimeInvoked : Bool
    , preferredDateInvoked : Bool
    , preferredTimeInvoked : Bool
    }

defaultOptions : FormatOptions
defaultOptions =
    { preferredDate = "%Y-%m-%d"
    , preferredTime = "%H:%M:%S"
    , preferredDateTime = "%Y-%m-%d %H:%M:%S"
    , amPmNames =
        (\ampm -> case ampm of
            AM -> "am"
            PM -> "pm"
        )
    , monthNames =
        (\month -> List.getAt (month - 1)
            [ "January", "February", "March", "April", "May", "June", "July"
            , "August", "September", "October", "November", "December"
            ]
        )
    , dayOfWeekNames =
        (\whichDay -> List.getAt (whichDay - 1)
            [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
            , "Saturday", "Sunday"
            ]
        )
    , abbreviatedMonthNames =
        (\month -> List.getAt (month - 1)
            [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"
            , "Oct", "Nov", "Dec"
            ]
        )
    , abbreviatedDayOfWeekNames =
        (\whichDay -> List.getAt (whichDay - 1)
            [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
        )
    , preferredDateInvoked = False
    , preferredTimeInvoked = False
    , preferredDateTimeInvoked = False
    }

type ParseError
    = Invalid
    | InvalidFormatSpecifier

parse : String -> DateTime -> FormatOptions -> Result ParseError String
parse stringFormat dateTime formatOptions =
    doParse (String.toList stringFormat) dateTime formatOptions []
        |> Result.map List.reverse
        |> Result.map List.concat
        |> Result.map String.fromList

doParse : List Char -> DateTime -> FormatOptions -> List (List Char) -> Result ParseError (List (List Char))
doParse stringFormat dateTime formatOptions acc =
    case stringFormat of
        [] ->
            Ok acc
        '%' :: rest ->
            parseModifiers rest Nothing Nothing (dateTime, formatOptions, acc)
        c :: rest ->
            doParse rest dateTime formatOptions ([ c ] :: acc)

parseModifiers : List Char -> Maybe Int -> Maybe Char -> (DateTime, FormatOptions, List (List Char)) -> Result ParseError (List (List Char))
parseModifiers stringFormat width pad ((dateTime, formatOptions, acc) as parser)  =
    case stringFormat of
        '-' :: rest ->
            parseModifiers rest width Nothing parser
        '0' :: rest ->
            parseModifiers rest width (Just '0') parser
        '_' :: rest ->
            parseModifiers rest width (Just ' ') parser
        c :: rest ->
            if Char.isDigit c then
                let
                    newWidth =
                        case width of
                            Nothing ->
                                Char.toCode c
                            Just w ->
                                w + Char.toCode c
                in
                parseModifiers rest (Just newWidth) pad parser
            else
                case (pad, width) of
                    (Nothing, _) ->
                        parseModifiers (c :: rest) width (Just <| defaultPad c) parser
                    (_, Nothing) ->
                        parseModifiers (c :: rest) (Just <| defaultWidth c) pad parser
                    (Just somePad, Just someWidth) ->
                        formatModifiers (c :: rest) someWidth somePad dateTime formatOptions acc
        rest ->
            doParse rest dateTime formatOptions acc

defaultPad : Char -> Char
defaultPad format =
    if String.contains (String.fromChar format) "aAbBpPZ" then
        ' '
    else
        '0'

defaultWidth : Char -> Int
defaultWidth format =
    if String.contains (String.fromChar format) "dHImMSy" then
        2
    else if format == 'j' then
        3
    else if List.member format ['Y', 'z'] then
        4
    else
        0


formatModifiers : List Char -> Int -> Char -> DateTime -> FormatOptions -> List (List Char) -> Result ParseError (List (List Char))
formatModifiers format width pad dateTime formatOptions acc =
    case format of
        '%' :: rest ->
            doParse rest dateTime formatOptions ( (padLeading width pad "%") :: acc)
        'a' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> Date.dayOfWeek dt.date)
                    |> formatOptions.abbreviatedDayOfWeekNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest dateTime formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'A' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> Date.dayOfWeek dt.date)
                    |> formatOptions.dayOfWeekNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest dateTime formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'b' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> Date.dayOfWeek dt.date)
                    |> formatOptions.abbreviatedMonthNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest dateTime formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'B' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> Date.dayOfWeek dt.date)
                    |> formatOptions.monthNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest dateTime formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'd' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> dt.date.day)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest dateTime formatOptions (result :: acc)
        'j' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> Date.dayOfYear dt.date)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest dateTime formatOptions (result :: acc)
        'm' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> dt.date.month)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest dateTime formatOptions (result :: acc)
        'u' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> Date.dayOfWeek dt.date)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest dateTime formatOptions (result :: acc)
        'y' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> remainderBy 100 dt.date.year)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest dateTime formatOptions (result :: acc)
        'Y' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> dt.date.year)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest dateTime formatOptions (result :: acc)
        _ ->
            Err InvalidFormatSpecifier

padLeading : Int -> Char -> String -> List Char
padLeading count padding string =
    let
        toPad = count - (String.length string)
    in
    if toPad > 0 then
        doPadLeading toPad padding (String.toList string)
    else
        String.toList string

doPadLeading : Int -> Char -> List Char -> List Char
doPadLeading count padding acc =
    if count == 0 then
        acc
    else
        doPadLeading (count - 1) padding (padding :: acc)