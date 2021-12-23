{-# LANGUAGE OverloadedStrings #-}

module Q.Time
        ( module Q.Time.Date
        , module Q.Time.DayCounter
        , parseDay
        , parseLocalTime
        , dayToString
        , dateToString
        ) where


import           Data.Time (Day, LocalTime (LocalTime), TimeOfDay, midnight)

import           Data.Time.Format.ISO8601 (Format, FormatExtension (BasicFormat), calendarFormat,
                                           formatParseM, formatShow, localTimeFormat,
                                           timeOfDayFormat)
import           Q.Time.Date
import           Q.Time.DayCounter

-- | Converts a shortened ISO08601 date string, or datetime to a 'LocalTime'.
-- If the string doesn't contain time then 'midnight' is used.
parseLocalTime :: String -> Maybe LocalTime
parseLocalTime iso_datetime =
  if length iso_datetime == 8 then do
    day <- formatParseM dayFormat' iso_datetime
    return $ LocalTime day midnight
  else
    formatParseM localTimeFormat' iso_datetime

-- | Converts a shortned ISO08601 date to a 'Day'
parseDay :: String -> Maybe Day
parseDay = formatParseM dayFormat'


-- | basic ISO08601 date/time format.
localTimeFormat' :: Format LocalTime
localTimeFormat' = localTimeFormat dayFormat' timeFormat'

-- | basic ISO08601 time format.
timeFormat' :: Format TimeOfDay
timeFormat' = timeOfDayFormat BasicFormat

-- | basic ISO08601 day format.
dayFormat' :: Format Day
dayFormat' = calendarFormat BasicFormat

-- | Format a date as an basic ISO08601 format.
dateToString :: LocalTime -> String
dateToString = formatShow localTimeFormat'

dayToString :: Day -> String
dayToString = formatShow dayFormat'
