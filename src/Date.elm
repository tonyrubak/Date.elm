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

-- Date/ISO Days Conversion