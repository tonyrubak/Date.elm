module Date exposing (..)

import DateTypes exposing (..)

dayOfWeek : Date -> Int
dayOfWeek ({ year, month, day } as date) =
    let
        (result, _, _) = date.calendar.dayOfWeek year month day
    in
    result

-- Date/ISO Days Conversion