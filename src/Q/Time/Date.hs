{-# LANGUAGE DeriveGeneric #-}
module Q.Time.Date (Calendar(..)) where

import Data.Time
import GHC.Generics

{- |Business Day conventions
 - These conventions specify the algorithm used to adjust a date in case it is not a valid business day.
 -}
data BusinessDayConvention =
          Following          -- ^Choose the first business day after the holiday 
        | ModifiedFollowing  {- ^Choose the first business day after
                                   the given holiday unless it belongs
                                    to a different month, in which case
                                    choose the first business day before
                                    the holiday -} 
        | Preceding          -- ^Choose the first business day before the holiday
        | ModifiedPreceding  {- ^Choose the first business day before
                                    the given holiday unless it belongs
                                    to a different month, in which case
                                    choose the first business day after
                                    the holiday. -}
        | Unadjusted         -- ^Do not adjust
        deriving (Generic, Show, Eq, Enum)

-- | Defines a holidays for given calendar. Corresponds to calendar class in QuantLib
class Calendar m where
  isHoliday :: m -> (Integer, Int, Int) -> Bool
  isWeekend :: m -> Day -> Bool

  isBusinessDay :: m -> Day -> Bool
  isBusinessDay m d = not (isHoliday m $ toGregorian d)

  hBusinessDayBetween :: m -> (Day, Day) -> Int
  hBusinessDayBetween m (fd, td) = foldl countDays 0 listOfDates
    where   countDays counter x     = counter + fromEnum (isBusinessDay m x)
            listOfDates             = getDaysBetween (fd, td)

  hNextBusinessDay :: m -> Day -> Day
  hNextBusinessDay m d | isBusinessDay m nextDay = nextDay
                       | otherwise                = getNextBusinessDay m nextDay
    where   nextDay = addDays 1 d



-- | Generate a list of all dates inbetween
getDaysBetween ::  (Day, Day) -> [Day]
getDaysBetween (fd, td) = reverse $ generator fd []
  where   generator date x
            | date < td     = generator nextDate (nextDate : x)
            | otherwise     = x
            where   nextDate        = addDays 1 date

-- | Gets the next working day
getNextBusinessDay :: Calendar a => a -> Day -> Day
getNextBusinessDay m d
  | isBusinessDay m nextDay       = nextDay
  | otherwise                     = getNextBusinessDay m nextDay
  where   nextDay = addDays 1 d

