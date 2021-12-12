module Q.Time.DayCounter
  (
    DayCounter(..)
  , dcName
  , dcYearFraction
  , dcCount
  ) where

import           Data.Time.Calendar
import           GHC.Generics


data DayCounter = Thirty360USA
                | Thirty360European
                | Thirty360Italian
                | Act365_25
                deriving stock (Generic, Eq, Show, Read)

dcName :: DayCounter -> String
dcName Thirty360USA      = "Thirty USA"
dcName Thirty360European = "Thirty Euro"
dcName Thirty360Italian  = "Thirty Italian"
dcName Act365_25         = "Act/365.25"

dcYearFraction :: Fractional p => DayCounter -> Day -> Day -> p
dcYearFraction dc@Thirty360USA fromDate toDate =
  fromIntegral (dcCount dc fromDate toDate) / 360.0
dcYearFraction dc@Thirty360European fromDate toDate =
  fromIntegral (dcCount dc fromDate toDate) / 360.0
dcYearFraction dc@Thirty360Italian fromDate toDate =
  fromIntegral (dcCount dc fromDate toDate) / 360.0
dcYearFraction dc@Act365_25 fromDate toDate =
  fromIntegral (dcCount dc fromDate toDate) / 365.25

dcCount :: DayCounter -> Day -> Day -> Int
dcCount Thirty360USA fd td = 360*(yy2-yy1) + 30*(mm2-mm1-1) + max 0 (30-dd1) + min 30 dd2
  where   (yy1, mm1, dd1) = intGregorian fd
          (yy2, m2, d2)   = intGregorian td
          (dd2, mm2)      = adjust dd1 d2 m2
          adjust x1 x2 z2
            | x2 == 31 && x1 < 30   = (1, z2+1)
            | otherwise             = (x2, z2)

dcCount Thirty360European fd td = 360*(yy2-yy1) + 30*(m2-m1-1) + max 0 (30-d1) + min 30 d2
  where   (yy1, m1, d1)    = intGregorian fd
          (yy2, m2, d2)    = intGregorian td

dcCount Thirty360Italian fd td = 360*(yy2-yy1) + 30*(mm2-mm1-1) + max 0 (30-dd1) + min 30 dd2
  where   (yy1, mm1, d1)   = intGregorian fd
          (yy2, mm2, d2)   = intGregorian td
          dd1              = adjust d1 mm1
          dd2              = adjust d2 mm2
          adjust x1 z1
            | z1 == 2 && x1 > 27    = 30
            | otherwise             = x1
dcCount Act365_25 fd td = fromIntegral $ diffDays td fd

intGregorian ::  Day -> (Int, Int, Int)
intGregorian date = (fromIntegral y, m, d)
  where (y, m, d) = toGregorian date
