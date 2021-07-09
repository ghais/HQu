{-# LANGUAGE OverloadedStrings #-}

module Q.Time
        ( module Q.Time.Date
        , module Q.Time.DayCounter
        , parseDay
        , parseLocalTime
        ) where

import qualified Data.ByteString          as B
import           Data.ByteString.Char8    (unpack)
import           Data.Csv                 (FromField (..), ToField (..), record,
                                           toField, (.!), (.:))
import           Data.Maybe               (fromJust)
import           Data.Time
import           Data.Time.Format
import           Data.Time.Format.ISO8601
import           Data.Vector              (Vector, toList)
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
localTimeFormat' = localTimeFormat dayFormat' timeFormat'
-- | basic ISO08601 time format.
timeFormat' = timeOfDayFormat BasicFormat
-- | basic ISO08601 day format.
dayFormat' = calendarFormat BasicFormat

-- | Format a date as an basic ISO08601 format.
dateToString :: LocalTime -> String
dateToString date = formatShow localTimeFormat' date


instance ToField Day where
   toField d = toField $ formatShow dayFormat' d
instance FromField Day where
  parseField s = pure $ fromJust (parseDay (unpack s))

