{-# LANGUAGE DeriveGeneric #-}
module Q.Time.Date
  (
    Calendar(..)
  ,isHoliday,isBusinessDay,businessDayBetween,nextBusinessDay) where

import           Data.Time
import           GHC.Generics

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
        deriving stock (Generic, Show, Eq, Enum)

data Calendar = NullGregorian -- ^ A gregorian calendar with no holidays. Weekends are Sat and Sunday


isHoliday :: Calendar -> Day -> Bool
isHoliday NullGregorian _ = False

isWeekend :: Calendar -> Day -> Bool
isWeekend NullGregorian d = isSaturday || isSunday
  where isSaturday = dow == Saturday
        isSunday   = dow == Sunday
        dow        = dayOfWeek d

isBusinessDay :: Calendar -> Day -> Bool
isBusinessDay cal d = not (isHoliday cal d && isWeekend cal d)

businessDayBetween :: Calendar -> (Day, Day) -> Int
businessDayBetween cal (fd, td) = foldl countDays 0 listOfDates
  where   countDays counter x     = counter + fromEnum (isBusinessDay cal x)
          listOfDates             = getDaysBetween (fd, td)

nextBusinessDay :: Calendar -> Day -> Day
nextBusinessDay m d | isBusinessDay m nextDay = nextDay
                     | otherwise                = getNextBusinessDay m nextDay
  where   nextDay = addDays 1 d



-- | Generate a list of all dates in between [fd, td)
getDaysBetween ::  (Day, Day) -> [Day]
getDaysBetween (fd, td) = reverse $ generator fd []
  where   generator date x
            | date < td     = generator nextDate (nextDate : x)
            | otherwise     = x
           where nextDate   = addDays 1 date

-- | Gets the next working day
getNextBusinessDay :: Calendar -> Day -> Day
getNextBusinessDay m d
  | isBusinessDay m nextDay       = nextDay
  | otherwise                     = getNextBusinessDay m nextDay
  where   nextDay = addDays 1 d

