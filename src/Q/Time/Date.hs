{-# LANGUAGE DeriveGeneric #-}
module Q.Time.Date
  (
    Calendar(..)
  ,isHoliday,isBusinessDay,businessDayBetween,nextBusinessDay) where


import qualified Data.Set as Set
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
        deriving stock (Generic, Show, Eq, Enum)

newtype SystematicHoliday    = SystematicHoliday [DayOfWeek]
newtype NonsystmeaticHoliday = NonsystmeaticHoliday (Set.Set Day)

isSystematicHoliday :: Day -> SystematicHoliday -> Bool
isSystematicHoliday d (SystematicHoliday holidays) = (dayOfWeek  d) `elem` holidays

isNonSystematicHoliday :: Day -> NonsystmeaticHoliday -> Bool
isNonSystematicHoliday d (NonsystmeaticHoliday holidays) = Set.member d holidays

data Calendar =
  -- | A gregorian calendar with no holidays. Weekends are Sat and Sunday
  NullGregorian
  -- | A calendar of systematic and non systematic holidays
  | CustomCal SystematicHoliday NonsystmeaticHoliday


isHoliday :: Day -> Calendar -> Bool
isHoliday  _ NullGregorian = False
isHoliday d (CustomCal _ nonsys) = isNonSystematicHoliday d nonsys

isWeekend :: Day -> Calendar -> Bool
isWeekend d NullGregorian = isSaturday || isSunday
  where isSaturday = dow == Saturday
        isSunday   = dow == Sunday
        dow        = dayOfWeek d
isWeekend d (CustomCal sys _)  = isSystematicHoliday d sys

isBusinessDay :: Day -> Calendar -> Bool
isBusinessDay d cal= not (isHoliday d cal && isWeekend d cal)

businessDayBetween :: Calendar -> Day -> Day -> Int
businessDayBetween cal fd td = foldl countDays 0 listOfDates
  where   countDays counter x     = counter + fromEnum (isBusinessDay x cal)
          listOfDates             = getDaysBetween fd td

nextBusinessDay :: Day -> Calendar -> Day
nextBusinessDay d cal | isBusinessDay nextDay cal = nextDay
                      | otherwise                = getNextBusinessDay nextDay cal
  where   nextDay = addDays 1 d



-- | Generate a list of all dates in between [fd, td)
getDaysBetween ::  Day -> Day -> [Day]
getDaysBetween fd td = reverse $ generator fd []
  where   generator date x
            | date < td     = generator nextDate (nextDate : x)
            | otherwise     = x
           where nextDate   = addDays 1 date

-- | Gets the next working day
getNextBusinessDay :: Day -> Calendar ->  Day
getNextBusinessDay d cal
  | isBusinessDay nextDay cal       = nextDay
  | otherwise                     = getNextBusinessDay nextDay cal
  where   nextDay = addDays 1 d

