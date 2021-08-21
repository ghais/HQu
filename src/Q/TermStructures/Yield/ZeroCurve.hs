module Q.TermStructures.Yield.ZeroCurve
  (
    Interpolated
  , mkZeroCurve
  , mkInterpolatedZeroCurve
  )
where
import qualified Data.SortedList as SortedList
import           Data.Time.Calendar (Day)
import qualified Q.Interpolation as Interp
import           Q.TermStructures
import           Q.Time (DayCounter)
import           Q.Time.Date
import           Q.Types
import Q.IR


data Interpolated = Interpolated
  {
    _refDate        :: Day
  , _dc             :: DayCounter
  , _calendar       :: Calendar
  , _settlementDays :: Int
  , _interpolator   :: Interp.Interpolation YearFrac Rate
  , _compounding    :: Compounding
  }

-- Make a discount curve where the interpolation is log linear.
mkZeroCurve ::
     Day                                    -- ^ Reference date
  -> DayCounter                             -- ^ Day counter
  -> Calendar                               -- ^ Calendar
  -> Int                                    -- ^ Settlement days
  -> SortedList.SortedList (YearFrac, Rate) -- ^ Yields . If the first discount factor is not (YearFrac 0, Rate 0)
                                            -- then it will be inserted.
  -> Compounding
  -> Either String Interpolated
mkZeroCurve d dc cal settlementDays rates comp = undefined

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

mkInterpolatedZeroCurve d dc cal sd f (SortedList.uncons  -> Nothing) comp = Left "No discount factors"

mkInterpolatedZeroCurve d dc cal sd f yields Continuous =
  case (SortedList.uncons yields) of
    Just ((d, _), _) -> Right Interpolated
                       {
                         _refDate = d
                       , _dc = dc
                       , _calendar = cal
                       , _settlementDays = sd
                       , _interpolator = f yields
                       , _compounding = Continuous
                       }
    otherwise  -> undefined
                         

{-
mkInterpolatedDiscountCurve d dc cal settlementDays interpolation yields comp=
  Right Interpolated
  {
    _refDate = d
  , _dc = dc
  , _calendar = cal
  , _settlementDays = settlementDays
  , _interpolator = interpolation
  , _yields = SortedList.nub (SortedList.insert  (YearFrac 0, Rate 0) yields)
  , _compounding = comp
  }
-}
instance TermStructure Interpolated where
  tsReferenceDate Interpolated{..} = _refDate
  tsCalendar Interpolated{..} = _calendar
  tsDayCounter Interpolated{..} = _dc
  tsSettlementDays Interpolated{..} = _settlementDays

instance YieldTermStructure Interpolated where
  yieldDiscountT Interpolated{..} t = undefined


