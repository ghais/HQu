module Q.TermStructures.Interpolated where

import Data.Time (Day)
import Q.Time
import Q.Types
import qualified Q.Interpolation as Interp

data Interpolated v = Interpolated
  {
    _refDate        :: Day
  , _dc             :: DayCounter
  , _calendar       :: Calendar
  , _settlementDays :: Int
  , _interpolator   :: Interp.Interpolation YearFrac v
  , _maxDate        :: Day
  }
