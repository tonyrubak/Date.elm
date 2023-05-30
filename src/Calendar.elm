module Calendar exposing (parse, FormatOptions, defaultOptions)

import Date
import DateTypes exposing (Date)
import List.Extra as List
import Array

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
        (\month -> Array.get (month - 1)
            <| Array.fromList
                [ "January", "February", "March", "April", "May"
                , "June", "July", "August", "September", "October", "November"
                , "December"
                ]
        )
    , dayOfWeekNames =
        (\whichDay -> Array.get(whichDay - 1)
            <| Array.fromList
                [ "Monday", "Tuesday", "Wednesday", "Thursday"
                , "Friday", "Saturday", "Sunday"
                ]
        )
    , abbreviatedMonthNames =
        (\month -> Array.get (month - 1)
            <| Array.fromList
                [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"
                , "Oct", "Nov", "Dec"
                ]
        )
    , abbreviatedDayOfWeekNames =
        (\whichDay -> Array.get (whichDay - 1)
            <| Array.fromList
                [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
        )
    , preferredDateInvoked = False
    , preferredTimeInvoked = False
    , preferredDateTimeInvoked = False
    }

type ParseError
    = Invalid
    | InvalidFormatSpecifier

parse : String -> Date -> FormatOptions -> Result ParseError String
parse stringFormat date formatOptions =
    doParse (String.toList stringFormat) date formatOptions []
        |> Result.map List.reverse
        |> Result.map List.concat
        |> Result.map String.fromList

doParse : List Char -> Date -> FormatOptions -> List (List Char) -> Result ParseError (List (List Char))
doParse stringFormat date formatOptions acc =
    case stringFormat of
        [] ->
            Ok acc
        '%' :: rest ->
            parseModifiers rest Nothing Nothing (date, formatOptions, acc)
        c :: rest ->
            doParse rest date formatOptions ([ c ] :: acc)

parseModifiers : List Char -> Maybe Int -> Maybe Char -> (Date, FormatOptions, List (List Char)) -> Result ParseError (List (List Char))
parseModifiers stringFormat width pad ((date, formatOptions, acc) as parser)  =
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
                        formatModifiers (c :: rest) someWidth somePad date formatOptions acc
        rest ->
            doParse rest date formatOptions acc

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


formatModifiers : List Char -> Int -> Char -> Date -> FormatOptions -> List (List Char) -> Result ParseError (List (List Char))
formatModifiers format width pad date formatOptions acc =
    case format of
        '%' :: rest ->
            doParse rest date formatOptions ( (padLeading width pad "%") :: acc)
        'a' :: rest ->
            let
                result =
                    date
                    |> Date.dayOfWeek
                    |> formatOptions.abbreviatedDayOfWeekNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest date formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'A' :: rest ->
            let
                result =
                    date
                    |> Date.dayOfWeek
                    |> formatOptions.dayOfWeekNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest date formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'b' :: rest ->
            let
                result =
                    date
                    |> (\dt -> dt.month)
                    |> formatOptions.abbreviatedMonthNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest date formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'B' :: rest ->
            let
                result =
                    date
                    |> (\dt -> dt.month)
                    |> formatOptions.monthNames
                    |> (Maybe.map <| padLeading width pad)
            in
            case result of
                Just someChar ->
                    doParse rest date formatOptions (someChar :: acc)
                Nothing ->
                    Err Invalid
        'd' :: rest ->
            let
                result =
                    date
                    |> (\dt -> dt.day)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest date formatOptions (result :: acc)
        'j' :: rest ->
            let
                result =
                    date
                    |> Date.dayOfYear
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest date formatOptions (result :: acc)
        'm' :: rest ->
            let
                result =
                    date
                    |> (\dt -> dt.month)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest date formatOptions (result :: acc)
        'u' :: rest ->
            let
                result =
                    date
                    |> Date.dayOfWeek
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest date formatOptions (result :: acc)
        'y' :: rest ->
            let
                result =
                    date
                    |> (\dt -> remainderBy 100 dt.year)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest date formatOptions (result :: acc)
        'Y' :: rest ->
            let
                result =
                    date
                    |> (\dt -> dt.year)
                    |> String.fromInt
                    |> padLeading width pad
            in
            doParse rest date formatOptions (result :: acc)
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