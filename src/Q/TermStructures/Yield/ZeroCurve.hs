{-# LANGUAGE FlexibleContexts #-}
module Q.TermStructures.Yield.ZeroCurve
  (
    Interpolated
  , mkZeroCurve
  , mkInterpolatedZeroCurve
  )
where
import           Data.Coerce (coerce)
import qualified Data.SortedList as SortedList
import           Data.Time.Calendar (Day)
import           Q.IR (Compounding (Continuous), InterestRate (InterestRate), equivalentRate, irRate)
import           Q.Interpolation (derivative, interpolate)
import qualified Q.Interpolation as Interp
import           Q.TermStructures (TermStructure (tsCalendar, tsDayCounter, tsMaxTime, tsReferenceDate, tsSettlementDays),
                                   YieldTermStructure, tsMaxDate, yieldDiscountT)
import           Q.Time.Date (Calendar)
import           Q.Time.DayCounter (DayCounter)
import           Q.Types (Derivative (..), Rate (..), YearFrac (YearFrac))
import Q.Time (dcYearFraction)
import Q.IR(discountFactor)


data Interpolated = Interpolated
  {
    _refDate        :: Day
  , _dc             :: DayCounter
  , _calendar       :: Calendar
  , _settlementDays :: Int
  , _interpolator   :: Interp.Interpolation YearFrac Rate
  , _compounding    :: Compounding
  , _maxDate        :: Day
  }

-- Make a discount curve where the interpolation is log linear.
mkZeroCurve ::
     Day                                    -- ^ Reference date
  -> DayCounter                             -- ^ Day counter
  -> Calendar                               -- ^ Calendar
  -> Int                                    -- ^ Settlement days
  -> SortedList.SortedList (Day, Rate)      -- ^ Yields . If the first discount factor is not (YearFrac 0, Rate 0)
                                            -- then it will be inserted.
  -> Compounding
  -> Either String Interpolated
mkZeroCurve d dc cal settlementDays = mkInterpolatedZeroCurve d dc cal settlementDays logLinear
  where logLinear vs = let (xs, ys) = unzip $ SortedList.fromSortedList vs
                           ts       = map (dcYearFraction dc d) xs
                       in Interp.LogInterp (Interp.linearInterpolator ts (coerce ys))

-- todo(0x47) Note sure i like this. A simple approach is just to define the interpolation method as an enum.
-- review interpolation again!!
type YieldCurveFactory = SortedList.SortedList (Day, Rate) -> Interp.Interpolation YearFrac Rate

-- | Make an interpolated discount curve.
mkInterpolatedZeroCurve ::
     Day                                            -- ^ Reference date
  -> DayCounter                                     -- ^ Day counter
  -> Calendar                                       -- ^ Calendar
  -> Int                                            -- ^ Settlement days
  -> YieldCurveFactory                              -- ^ Interpolation method.
  -> SortedList.SortedList (Day, Rate)              -- ^ Discount factors. If the first discount factor is not (YearFrac 0, Yield 0)
                                                    -- then it will be inserted.
  -> Compounding                                    -- ^ Compounding
  -> Either String Interpolated

mkInterpolatedZeroCurve _ _ _ _ _ (SortedList.uncons  -> Nothing) _ = Left "No discount factors"

mkInterpolatedZeroCurve d0 dc cal sd f yields@(SortedList.uncons -> Just((d, r), _)) Continuous =
  if d == d0 then
    Right Interpolated
    {
      _refDate = d0
    , _dc = dc
    , _calendar = cal
    , _settlementDays = sd
    , _interpolator = f yields
    , _compounding = Continuous
    , _maxDate     = (fst . last . SortedList.fromSortedList) yields
    }
  else
    mkInterpolatedZeroCurve d0 dc cal sd f (SortedList.insert (d0, r) yields) Continuous


mkInterpolatedZeroCurve d0 dc cal sd f yields comp = mkInterpolatedZeroCurve d0 dc cal sd f (SortedList.map toContinuous yields) Continuous
  where toContinuous (d, r) = (d, irRate (equivalentRate (InterestRate r dc comp) Continuous (dcYearFraction dc d0 d)))

instance TermStructure Interpolated where
  tsReferenceDate Interpolated{..} = _refDate
  tsCalendar Interpolated{..} = _calendar
  tsDayCounter Interpolated{..} = _dc
  tsSettlementDays Interpolated{..} = _settlementDays
  tsMaxDate Interpolated{..} = _maxDate


yieldZeroRateT :: Interpolated -> YearFrac -> Compounding -> InterestRate
yieldZeroRateT  ts@Interpolated{..} t compounding =
    if t <= YearFrac tMax then
      let z = InterestRate (interpolate _interpolator t) _dc _compounding
      in equivalentRate z compounding t
    else
      let (Rate zMax) = interpolate _interpolator (YearFrac tMax)
          (YearFrac dt) = t - coerce tMax
          d :: Derivative YearFrac Rate
          d = derivative _interpolator (YearFrac tMax)
          instFwdMax = zMax + tMax * coerce d
          z = InterestRate  (Rate ((zMax * tMax + instFwdMax * dt) / coerce t)) _dc _compounding
      in equivalentRate z compounding t
    where (YearFrac tMax) = tsMaxTime ts

instance YieldTermStructure Interpolated where
  yieldDiscountT ts t = let z = yieldZeroRateT ts t Continuous
                        in discountFactor z t
  


