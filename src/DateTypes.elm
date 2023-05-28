module DateTypes exposing (..)

type alias Date =
    { year : Int
    , month : Int
    , day : Int
    , calendar : Calendar
    }

type alias Time =
    { hour : Int
    , minute : Int
    , second : Int
    }

type alias DateTime =
    { date: Date
    , time: Time
    }

type alias Calendar =
    { dayOfWeek : Int -> Int -> Int -> (Int, Int, Int)
    , daysBeforeMonth : Int -> Int
    , leapDayOffset : Int -> Int -> Int
    , isLeapYear : Int -> Bool
    , daysInMonth : Int -> Int -> Int
    , monthsInYear : Int -> Int
    }