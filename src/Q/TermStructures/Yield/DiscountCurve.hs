module Q.TermStructures.Yield.DiscountCurve
  (
    Interpolated
  , ConstRate(..)
  , mkDiscountCurve
  , mkInterpolatedDiscountCurve
  , mkConstantDiscountCurve
  , mkRightContiniousDiscountCurve
  ) where

import           Data.Coerce (coerce)
import qualified Data.SortedList as SortedList
import           Data.Time.Calendar (Day, fromGregorian)
import           Q.Interpolation (Interpolation (Const), Interpolator (derivative, interpolate),
                                  logLinearInterpolator, rightContinuousPieceWiseConstant)
import           Q.TermStructures (TermStructure (..),
                                   YieldTermStructure (yieldDiscountT))
import           Q.Time (DayCounter, dcYearFraction)

import           Q.Types (DF (..), Derivative (..), YearFrac (YearFrac), Rate(..))
import GHC.Generics (Generic)

data Interpolated = Interpolated
  {
    _refDate        :: Day
  , _dc             :: DayCounter
  , _interpolator   :: Interpolation YearFrac DF
  , _maxDate        :: Day
  }

newtype ConstRate = ConstRate Rate deriving stock (Generic, Show)

type DiscountCurveFactory = SortedList.SortedList (Day, DF) -> Interpolation YearFrac DF


-- |Make a discount curve where the interpolation is log linear.
mkDiscountCurve ::
    Day                                   -- ^ Reference date
  -> DayCounter                            -- ^ Day counter
  -> SortedList.SortedList (Day, DF)       -- ^ Discount factors. If the first discount factor is not (refDate, DF 1)
                                          --   then it will be inserted.
  -> Either String Interpolated
mkDiscountCurve d dc  = mkInterpolatedDiscountCurve d dc (mkInterpolator logLinearInterpolator d dc)


mkRightContiniousDiscountCurve ::
    Day
  -> DayCounter
  -> SortedList.SortedList (Day, DF)
  -> Either String Interpolated
mkRightContiniousDiscountCurve d dc = mkInterpolatedDiscountCurve d dc (mkInterpolator rightContinuousPieceWiseConstant d dc)



mkInterpolator :: ([YearFrac] -> [DF] -> Interpolation YearFrac DF) -> Day -> DayCounter -> SortedList.SortedList (Day, DF) -> Interpolation YearFrac DF
mkInterpolator f d dc vs = let (days, dfs) = unzip $ SortedList.fromSortedList vs
                               ts          = map (dcYearFraction dc d) days
                           in f ts dfs

mkConstantDiscountCurve ::
    Day                                   -- ^ Reference date
  -> DayCounter                            -- ^ Day counter
  -> DF                                    -- ^ The discount factor to use
  -> Interpolated
mkConstantDiscountCurve d dc (DF df)  = Interpolated d dc (Const df) (fromGregorian 3000 1 1) -- use a very large date for max date. 


-- | Make an interpolated discount curve.
mkInterpolatedDiscountCurve ::
    Day                                   -- ^ Reference date
  -> DayCounter                            -- ^ Day counter
  -> DiscountCurveFactory                  -- ^ Interpolation method factory
  -> SortedList.SortedList (Day, DF)       -- ^ Discount factors. If the first discount factor is not (refDate, DF 1)
                                          --   then it will be inserted.
  -> Either String Interpolated
mkInterpolatedDiscountCurve _ _  _ (SortedList.uncons  -> Nothing) = Left "No discount factors"
mkInterpolatedDiscountCurve d0 dc interpolation dfs@(SortedList.uncons -> Just ((d, _), _)) =
  if d <= d0 then
    Right Interpolated
    {
      _refDate = d0
    , _dc = dc
    , _interpolator = interpolation dfs
    , _maxDate =  (fst . last . SortedList.fromSortedList ) dfs
    }
  else
    mkInterpolatedDiscountCurve d0 dc interpolation (SortedList.insert  (d0, DF 1) dfs)

mkInterpolatedDiscountCurve d dc interpolation dfs =
  mkInterpolatedDiscountCurve d dc interpolation (SortedList.insert  (d, DF 1) dfs)

instance TermStructure Interpolated where
  tsReferenceDate Interpolated{..} = _refDate
  tsDayCounter Interpolated{..} = _dc
  tsMaxDate Interpolated{..} = Just _maxDate

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
    where tMax = dcYearFraction _dc _refDate _maxDate



instance YieldTermStructure ConstRate where
  yieldDiscountT (ConstRate (Rate r)) (YearFrac t) = DF $ exp (negate (r * t))

-- TODO
-- 1. Should i make Interpolated _maxDate a Maybe since the constant curve doesn't need one?
