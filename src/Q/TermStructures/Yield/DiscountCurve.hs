module Q.TermStructures.Yield.DiscountCurve
  (
    Interpolated
  , mkDiscountCurve
  , mkInterpolatedDiscountCurve
  ) where

import           Data.Coerce (coerce)
import qualified Data.SortedList as SortedList
import           Data.Time.Calendar (Day)
import           Q.Interpolation
import           Q.TermStructures
import           Q.Time (DayCounter, dcYearFraction)
import           Q.Time.Date
import           Q.Types




data Interpolated = Interpolated
  {
    _refDate        :: Day
  , _dc             :: DayCounter
  , _calendar       :: Calendar
  , _settlementDays :: Int
  , _interpolator   :: Interpolation YearFrac DF
  , _maxDate        :: Day
  }

type DiscountCurveFactory = SortedList.SortedList (Day, DF) -> Interpolation YearFrac DF

-- |Make a discount curve where the interpolation is log linear.
mkDiscountCurve ::
     Day                                   -- ^ Reference date
  -> DayCounter                            -- ^ Day counter
  -> Calendar                              -- ^ Calendar
  -> Int                                   -- ^ Settlement days
  -> SortedList.SortedList (Day, DF)       -- ^ Discount factors. If the first discount factor is not (refDate, DF 1)
                                           -- then it will be inserted.
  -> Either String Interpolated
mkDiscountCurve d dc cal settlementDays = mkInterpolatedDiscountCurve d dc cal settlementDays f
  where
    f :: DiscountCurveFactory
    f vs = let (days, dfs) = unzip $ SortedList.fromSortedList vs
               ts          = map (dcYearFraction dc d) days
               in logLinearInterpolator ts dfs

-- | Make an interpolated discount curve.
mkInterpolatedDiscountCurve ::
     Day                                   -- ^ Reference date
  -> DayCounter                            -- ^ Day counter
  -> Calendar                              -- ^ Calendar
  -> Int                                   -- ^ Settlement days
  -> DiscountCurveFactory                  -- ^ Interpolation method factory
  -> SortedList.SortedList (Day, DF)       -- ^ Discount factors. If the first discount factor is not (refDate, DF 1)
                                           -- then it will be inserted.
  -> Either String Interpolated
mkInterpolatedDiscountCurve _ _ _ _ _ (SortedList.uncons  -> Nothing) = Left "No discount factors"
mkInterpolatedDiscountCurve d0 dc cal settlementDays interpolation dfs@(SortedList.uncons -> Just ((d, _), _)) =
  if d <= d0 then
    Right Interpolated
    {
      _refDate = d0
    , _dc = dc
    , _calendar = cal
    , _settlementDays = settlementDays
    , _interpolator = interpolation dfs
    , _maxDate =  (fst . last . SortedList.fromSortedList ) dfs
    }
  else
    mkInterpolatedDiscountCurve d0 dc cal settlementDays interpolation (SortedList.insert  (d0, DF 1) dfs)

mkInterpolatedDiscountCurve d dc cal settlementDays interpolation dfs =
  mkInterpolatedDiscountCurve d dc cal settlementDays interpolation (SortedList.insert  (d, DF 1) dfs)

instance TermStructure Interpolated where
  tsReferenceDate Interpolated{..} = _refDate
  tsCalendar Interpolated{..} = _calendar
  tsDayCounter Interpolated{..} = _dc
  tsSettlementDays Interpolated{..} = _settlementDays
  tsMaxDate Interpolated{..} = _maxDate

instance YieldTermStructure Interpolated where
  yieldDiscountT ts@Interpolated{..} t =
    if t <= tMax then
      interpolate _interpolator  t
    else
      let (DF dMax) = yieldDiscountT ts tMax
          instFwdMax = coerce d/dMax
          (YearFrac dt) = t - tMax
          d :: Derivative YearFrac DF
          d = derivative _interpolator tMax
      in DF $ dMax * exp (instFwdMax * dt)
    where tMax = tsMaxTime ts





