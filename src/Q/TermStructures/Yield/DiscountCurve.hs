module Q.TermStructures.Yield.DiscountCurve
  (
    Interpolated
  , mkDiscountCurve
  , mkInterpolatedDiscountCurve
  )
where
import qualified Data.SortedList as SortedList
import           Data.Time.Calendar (Day)
import           Q.Interpolation
import           Q.TermStructures
import           Q.Time (DayCounter, dcYearFraction)
import           Q.Time.Date
import           Q.Types
import Data.Coerce (coerce)




data Interpolated = Interpolated
  {
    _refDate        :: Day
  , _dc             :: DayCounter
  , _calendar       :: Calendar
  , _settlementDays :: Int
  , _interpolator   :: Interpolation YearFrac DF
  , _maxDate        :: Day
  }

-- Make a discount curve where the interpolation is log linear.
mkDiscountCurve ::
     Day                                   -- ^ Reference date
  -> DayCounter                            -- ^ Day counter
  -> Calendar                              -- ^ Calendar
  -> Int                                   -- ^ Settlement days
  -> SortedList.SortedList (Day, DF)       -- ^ Discount factors. If the first discount factor is not (refDate, DF 1)
                                           -- then it will be inserted.
  -> Either String Interpolated
mkDiscountCurve d dc cal settlementDays discounts = mkInterpolatedDiscountCurve d dc cal settlementDays (LogInterp (linearInterpolator ts (coerce dfs))) discounts
  where (days, dfs) = unzip $ SortedList.fromSortedList discounts
        ts          = map (dcYearFraction dc d) days

-- | Make an interpolated discount curve.
mkInterpolatedDiscountCurve ::
     Day                                   -- ^ Reference date
  -> DayCounter                            -- ^ Day counter
  -> Calendar                              -- ^ Calendar
  -> Int                                   -- ^ Settlement days
  -> Interpolation YearFrac DF                        -- ^ Interpolation method.
  -> SortedList.SortedList (Day, DF)       -- ^ Discount factors. If the first discount factor is not (refDate, DF 1)
                                           -- then it will be inserted.
  -> Either String Interpolated
mkInterpolatedDiscountCurve _ _ _ _ _ (SortedList.uncons  -> Nothing) = Left "No discount factors"
mkInterpolatedDiscountCurve d0 dc cal settlementDays interpolation dfs@(SortedList.uncons -> Just ((d, DF 1), _)) =
  Right Interpolated
  {
    _refDate = d
  , _dc = dc
  , _calendar = cal
  , _settlementDays = settlementDays
  , _interpolator = interpolation
  , _maxDate =  (fst. last . SortedList.fromSortedList ) dfs
  }
mkInterpolatedDiscountCurve d dc cal settlementDays interpolation dfs =
  mkInterpolatedDiscountCurve d dc cal settlementDays interpolation (SortedList.insert  (d, DF 1) dfs)

instance TermStructure Interpolated where
  tsReferenceDate Interpolated{..} = _refDate
  tsCalendar Interpolated{..} = _calendar
  tsDayCounter Interpolated{..} = _dc
  tsSettlementDays Interpolated{..} = _settlementDays
  tsMaxDate Interpolated{..} = _maxDate

instance YieldTermStructure Interpolated where
  yieldDiscountT Interpolated{..} t = interpolate _interpolator  t


