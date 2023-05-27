module Iso exposing (..)

import DateTypes exposing (..)
import Parser exposing (Parser, (|.), (|=), getChompedString, chompIf, succeed)

isoCalendar : Calendar
isoCalendar =
    { dayOfWeek = dayOfWeek }

daysPerNonleapYear : Int
daysPerNonleapYear = 365
daysPerLeapYear : Int
daysPerLeapYear = 366

-- Convert year, month, day to days since 0000-00-01

dateToIsoDays : Int -> Int -> Int -> Int
dateToIsoDays year month day =
    if (year,month,day) == (0,1,1) then
        0
    else if (year,month,day) == (1970,1,1) then
        719528
    else
        daysInPreviousYears year + daysBeforeMonth month + leapDayOffset year month + day - 1

-- Convert days since 0000-00-01 to year, month, day

dateFromIsoDays : Int -> (Int, Int, Int)
dateFromIsoDays days =
    let
        (year, dayOfYear) = daysToYear days
        extraDay = if isLeapYear year then 1 else 0
        (month, dayInMonth) = yearDayToYearDate extraDay dayOfYear
    in
    (year, month, (dayInMonth + 1))


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

daysToYear : Int -> (Int, Int)
daysToYear days =
    let
        yearEstimate = days // daysPerNonleapYear
        (year, daysBeforeYear) = daysToYearHelper yearEstimate days <| daysInPreviousYears yearEstimate
    in
    (year, days - daysBeforeYear)

daysToYearHelper : Int -> Int -> Int -> (Int, Int)
daysToYearHelper year days1 days2 =
    if year >= 0 && days1 < days2 then
        daysToYearHelper (year - 1) days1 <| daysInPreviousYears (year - 1)
    else
        (year, days2)

yearDayToYearDate : Int -> Int -> (Int, Int)
yearDayToYearDate extraDay dayOfYear =
    if dayOfYear < 31 then
        (1, dayOfYear)
    else if dayOfYear < 59 + extraDay then
        (2, dayOfYear - 31)
    else if dayOfYear < 90 + extraDay then
        (3, dayOfYear - (59 + extraDay))
    else if dayOfYear < 120 + extraDay then
        (4, dayOfYear - (90 + extraDay))
    else if dayOfYear < 151 + extraDay then
        (5, dayOfYear - (120 + extraDay))
    else if dayOfYear < 181 + extraDay then
        (6, dayOfYear - (151 + extraDay))
    else if dayOfYear < 212 + extraDay then
        (7, dayOfYear - (181 + extraDay))
    else if dayOfYear < 243 + extraDay then
        (8, dayOfYear - (212 + extraDay))
    else if dayOfYear < 273 + extraDay then
        (9, dayOfYear - (243 + extraDay))
    else if dayOfYear < 304 + extraDay then
        (10, dayOfYear - (273 + extraDay))
    else if dayOfYear < 334 + extraDay then
        (11, dayOfYear - (304 + extraDay))
    else
        (12, dayOfYear - (334 + extraDay))

-- ISO Calendar Stuff

dayOfWeek : Int -> Int -> Int -> (Int, Int, Int)
dayOfWeek year month day =
    let
        isoDays = dateToIsoDays year month day
    in
        (isoDaysToDayOfWeek isoDays, 1, 7)

isoDaysToDayOfWeek : Int -> Int
isoDaysToDayOfWeek isoDays =
    1 + remainderBy isoDays 7

-- Parsing String Dates to Date Objects
dateFromParts : DateParts -> Date
dateFromParts { y1, y2, y3, y4, m1, m2, d1, d2 } =
    { year = y1 * 1000 + y2 * 100 + y3 * 10 + y4
    , month = m1 * 10 + m2
    , day = d1 * 10 + d2
    , calendar = isoCalendar
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