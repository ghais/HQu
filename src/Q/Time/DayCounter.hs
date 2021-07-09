{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Q.Time.DayCounter (
  DayCounter(..),
  Thirty360(..)
  ) where

import           Data.Time.Calendar
import           GHC.Generics
-- |Day counter type class
class DayCounter m where
  dcName         :: m -> String -- ^Name of the day counter.
  dcCount        :: m -> Day -> Day -> Int -- ^Number of business days inbetween
  dcYearFraction :: m -> Day -> Day -> Double -- ^Year fraction between 2 dates.


-- | Thirty day counters as in QuantLib
data Thirty360 = ThirtyUSA | ThirtyEuropean | ThirtyItalian
  deriving (Generic, Eq, Show, Read)

instance DayCounter Thirty360 where
  dcName ThirtyUSA      = "Thirty USA"
  dcName ThirtyEuropean = "Thirty Euro"
  dcName ThirtyItalian  = "Thirty Italian"

  dcYearFraction  dc fromDate toDate = fromIntegral (dcCount dc fromDate toDate) / 360.0

  dcCount ThirtyUSA fd td = 360*(yy2-yy1) + 30*(mm2-mm1-1) + max 0 (30-dd1) + min 30 dd2
    where   (yy1, mm1, dd1) = intGregorian fd
            (yy2, m2, d2)   = intGregorian td
            (dd2, mm2)      = adjust dd1 d2 m2
            adjust x1 x2 z2
              | x2 == 31 && x1 < 30   = (1, z2+1)
              | otherwise             = (x2, z2)


  dcCount ThirtyEuropean fd td = 360*(yy2-yy1) + 30*(m2-m1-1) + max 0 (30-d1) + min 30 d2
    where   (yy1, m1, d1)    = intGregorian fd
            (yy2, m2, d2)    = intGregorian td

  dcCount ThirtyItalian fd td = 360*(yy2-yy1) + 30*(mm2-mm1-1) + max 0 (30-dd1) + min 30 dd2
    where   (yy1, mm1, d1)   = intGregorian fd
            (yy2, mm2, d2)   = intGregorian td
            dd1              = adjust d1 mm1
            dd2              = adjust d2 mm2
            adjust x1 z1
              | z1 == 2 && x1 > 27    = 30
              | otherwise             = x1

intGregorian ::  Day -> (Int, Int, Int)
intGregorian date = (fromIntegral y, m, d)
  where (y, m, d) = toGregorian date
