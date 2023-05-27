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

parse : String -> DateTime -> FormatOptions -> List Char -> String
parse stringFormat dateTime formatOptions acc =
    case String.toList stringFormat of
        [] ->
            String.fromList <| List.reverse acc
        '%' :: rest ->
            parseModifiers (String.fromList rest) Nothing Nothing (dateTime, formatOptions, acc)
        c :: rest ->
            parse (String.fromList rest) dateTime formatOptions (c :: acc)

parseModifiers : String -> Maybe Int -> Maybe Char -> (DateTime, FormatOptions, List Char) -> String
parseModifiers stringFormat width pad ((dateTime, formatOptions, acc) as parser)  =
    case String.toList stringFormat of
        '-' :: rest ->
            parseModifiers (String.fromList rest) width Nothing parser
        '0' :: rest ->
            parseModifiers (String.fromList rest) width (Just '0') parser
        '_' :: rest ->
            parseModifiers (String.fromList rest) width (Just ' ') parser
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
                parseModifiers (String.fromList rest) (Just newWidth) pad parser
            else
                case (pad, width) of
                    (Nothing, _) ->
                        parseModifiers (String.fromList <| c :: rest) width (Just <| defaultPad c) parser
                    (_, Nothing) ->
                        parseModifiers (String.fromList <| c :: rest) (Just <| defaultWidth c) pad parser
                    (Just somePad, Just someWidth) ->
                        String.fromList <| formatModifiers (c :: rest) someWidth somePad dateTime formatOptions acc

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

formatModifiers : List Char -> Int -> Char -> DateTime -> FormatOptions -> List Char -> String
formatModifiers format width pad dateTime formatOptions acc =
    case format of
        '%' :: rest ->
            parse (String.fromList rest) dateTime formatOptions (padLeading width pad "%" ++ acc)
        'a' :: rest ->
            let
                result =
                    dateTime
                    |> (\dt -> Date.dayOfWeek dt.date)
                    |> formatOptions.abbreviatedDayOfWeekNames
                    |> padLeading width pad
            in
            parse (String.fromList rest) dateTime formatOptions (result ++ acc)
        _ ->
            parse (String.fromList format) dateTime formatOptions acc

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