module Date exposing (..)

import Parser exposing (Parser, (|.), (|=), getChompedString, chompIf, succeed)

daysPerNonleapYear : Int
daysPerNonleapYear = 365
daysPerLeapYear : Int
daysPerLeapYear = 366

type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }

type alias Time =
    { hour : Int
    , minute : Int
    , second : Int
    }

dateFromParts : DateParts -> Date
dateFromParts { y1, y2, y3, y4, m1, m2, d1, d2 } =
    { year = y1 * 1000 + y2 * 100 + y3 * 10 + y4
    , month = m1 * 10 + m2
    , day = d1 * 10 + d2
    }

timeFromParts : TimeParts -> Time
timeFromParts { h1, h2, m1, m2, s1, s2 } =
    { hour = h1 * 10 + h2
    , minute = m1 * 10 + m2
    , second = s1 * 10 + s2
    }

type alias DateParts =
    { y1 : Int
    , y2 : Int
    , y3 : Int
    , y4 : Int
    , m1 : Int
    , m2 : Int
    , d1 : Int
    , d2 : Int
    }

type alias TimeParts =
    { h1 : Int
    , h2 : Int
    , m1 : Int
    , m2 : Int
    , s1 : Int
    , s2 : Int
    }

type alias DateTime =
    { date : Date
    , time : Time
    }

digit : Parser Int
digit =
    Parser.map (Maybe.withDefault 0 << String.toInt) <| getChompedString <| chompIf Char.isDigit

date : Parser DateParts
date =
    succeed DateParts
        |= digit
        |= digit
        |= digit
        |= digit
        |. dateSep
        |= digit
        |= digit
        |. dateSep
        |= digit
        |= digit

time : Parser TimeParts
time =
    succeed TimeParts
        |= digit
        |= digit
        |. timeSep
        |= digit
        |= digit
        |. timeSep
        |= digit
        |= digit

datetime : Parser DateTime
datetime =
    succeed DateTime
        |= Parser.map dateFromParts date
        |. datetimeSep
        |= Parser.map timeFromParts time

isWhitespace : Char -> Bool
isWhitespace char =
    char == ' '
    || char == '\n'
    || char == '\r'
    || char == '\t'

isDatetimeSep : Char -> Bool
isDatetimeSep char =
    char == 'T' || isWhitespace char

datetimeSep : Parser String
datetimeSep =
    getChompedString <| chompIf isDatetimeSep

dateSep : Parser String
dateSep =
    getChompedString <| chompIf ( \c -> c == '-' )

timeSep : Parser String
timeSep =
    getChompedString <| chompIf ( \c -> c == ':' )

dateToIsoDays : Date -> Int
dateToIsoDays {year, month, day} =
    if (year,month,day) == (0,1,1) then
        0
    else if (year,month,day) == (1970,1,1) then
        719528
    else
        daysInPreviousYears year + daysBeforeMonth month + leapDayOffset year month + day - 1

dateFromIsoDays : Int -> Date
dateFromIsoDays days =
    let
        (year, dayOfYear) = daysToYear days
        extraDay = if isLeapYear year then 1 else 0
        (month, dayInMonth) = yearDayToYearDate extraDay dayOfYear
    in
    Date year month (dayInMonth + 1)


daysInPreviousYears : Int -> Int
daysInPreviousYears year =
    if year == 0 then
        0
    else
        let
            previousYear = year - 1
        in
        previousYear // 4 - previousYear // 100 + previousYear // 400 + previousYear * daysPerNonleapYear + daysPerLeapYear

daysBeforeMonth : Int -> Int
daysBeforeMonth month =
    if month == 1 then 0
    else if month == 2 then 31
    else if month == 3 then 59
    else if month == 4 then 90
    else if month == 5 then 120
    else if month == 6 then 151
    else if month == 7 then 181
    else if month == 8 then 212
    else if month == 9 then 243
    else if month == 10 then 273
    else if month == 11 then 304
    else if month == 12 then 334
    else 0

leapDayOffset : Int -> Int -> Int
leapDayOffset year month =
    if month < 3 then 0
    else if isLeapYear(year) then 1
    else 0

isLeapYear : Int -> Bool
isLeapYear year =
    remainderBy year 4 == 0 && (remainderBy year 100 /= 0 || remainderBy year 400 == 0)