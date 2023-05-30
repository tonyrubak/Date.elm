module Date exposing (..)

import DateTypes exposing (..)

dayOfWeek : Date -> Int
dayOfWeek ( { year, month, day } as date ) =
    let
        (result, _, _) = date.calendar.dayOfWeek year month day
    in
    result

dayOfYear : Date -> Int
dayOfYear {calendar, year, month, day } =
    ( calendar.daysBeforeMonth month )
    + ( calendar.leapDayOffset year month )
    + day

isLeapYear : Date -> Bool
isLeapYear ( { calendar, year } ) =
    calendar.isLeapYear year

daysInMonth : Date -> Int
daysInMonth ( { calendar, year, month } ) =
    calendar.daysInMonth year month

compare : Date -> Date -> Order
compare date1 date2 =
    if ( date1.year, date1.month, date1.day ) > ( date2.year, date2.month , date2.day ) then
        GT
    else if ( date1.year, date1.month, date1.day ) < ( date2.year, date2.month , date2.day ) then
        LT
    else
        EQ

add : Int -> Date -> Date
add days ({ calendar, year, month, day } as date) =
    let
        ( newYear, newMonth, newDay ) = calendar.addDays days year month day
    in
    { date | year = newYear, month = newMonth, day = newDay }

diff : Calendar -> Date -> Date -> Int
diff calendar day1 day2 =
    calendar.diff (day1.year, day1.month, day1.day) (day2.year, day2.month, day2.day)