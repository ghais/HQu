{-# LANGUAGE QuantifiedConstraints #-}
module Q.TermStructures
  where

import           Data.Coerce (coerce)
import           Data.Time (Day)
import           Q.IR (Compounding (..), InterestRate (InterestRate), impliedRate)
import           Q.Time.DayCounter (DayCounter, dcYearFraction)
import           Q.Types (DF (DF), Rate (Rate), YearFrac (YearFrac), Forward, Spot, rateFromDiscount, discountFactor)


class TermStructure ts  where
  tsReferenceDate :: ts -> Day
  tsDayCounter :: ts -> DayCounter
  tsMaxDate :: ts -> Maybe Day

  tsMaxTime :: ts -> Maybe YearFrac
  tsMaxTime ts = fmap (dcYearFraction (tsDayCounter ts) (tsReferenceDate ts)) (tsMaxDate ts)

  tsTimeFromReference :: ts -> Day -> YearFrac
  tsTimeFromReference ts = dcYearFraction dc  refDate
    where dc = tsDayCounter ts
          refDate = tsReferenceDate ts


class YieldTermStructure ts where
  yieldDiscountT :: ts -> YearFrac -> DF
  yieldDiscountT ts t = discountFactor t r
    where r = yieldContinuousZeroRateT ts t

  yieldContinuousZeroRateT :: ts -> YearFrac -> Rate
  yieldContinuousZeroRateT ts t =  rateFromDiscount t df
    where df = yieldDiscountT ts t


data NoDiscounting = NoDiscounting
instance YieldTermStructure NoDiscounting where
  yieldDiscountT _ _ = 1

class ForwardCurveTermStructure ts where
  tsSpot :: ts -> Spot
  tsForwardT :: ts -> YearFrac -> Forward

yieldDiscount :: (YieldTermStructure ts, TermStructure ts) => ts -> Day -> DF
yieldDiscount ts day = yieldDiscountT ts t
    where t = tsTimeFromReference ts day

yieldZeroRateT ::  (YieldTermStructure ts, TermStructure ts) => ts -> YearFrac -> Compounding -> InterestRate
yieldZeroRateT ts t comp
    | t < dt = yieldZeroRateT ts dt comp
    | otherwise = InterestRate compound (tsDayCounter ts) comp
    where compound = Rate  (recip df)
          (DF df)  = yieldDiscountT ts t

yieldZeroRate ::  (YieldTermStructure ts, TermStructure ts) => ts -> Day -> Compounding -> InterestRate
yieldZeroRate ts d = yieldZeroRateT ts t
    where t = tsTimeFromReference ts d

yieldForwardRateT ::  (YieldTermStructure ts, TermStructure ts) => ts -> YearFrac -> YearFrac -> Compounding -> InterestRate
yieldForwardRateT ts t1 t2 comp = impliedRate compound dc comp (t2' - t1')
    where dc = tsDayCounter ts
          (t1', t2') = if t1 == t2 then
                         (max (t1 - dt/2.0) 0, t1 + dt)
                       else
                         (t1, t2)
          compound = coerce (yieldDiscountT ts t1' / yieldDiscountT ts t2')

yieldForwardRate ::  (YieldTermStructure ts, TermStructure ts) => ts -> Day -> Day -> Compounding -> InterestRate
yieldForwardRate ts d1 d2 = yieldForwardRateT ts t1 t2
    where t1 = tsTimeFromReference ts d1
          t2 = tsTimeFromReference ts d2



dt :: YearFrac
dt = YearFrac 0.0001
