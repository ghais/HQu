{-# LANGUAGE QuantifiedConstraints #-}
module Q.TermStructures
  where

import           Data.Time
import           GHC.Base (coerce)
import           Q.IR
import           Q.Time.DayCounter
import           Q.Types (DF (DF), Rate (Rate), YearFrac (YearFrac))
import Q.Time.Date

class TermStructure ts  where
  tsReferenceDate :: ts -> Day
  tsCalendar :: ts -> Calendar
  tsDayCounter :: ts -> DayCounter
  tsSettlementDays :: ts -> Int
  tsMaxDate :: ts -> Day

  tsMaxTime :: ts -> YearFrac
  tsMaxTime ts = dcYearFraction (tsDayCounter ts) (tsReferenceDate ts) (tsMaxDate ts)

  tsTimeFromReference :: ts -> Day -> YearFrac
  tsTimeFromReference ts = dcYearFraction dc  refDate
    where dc = tsDayCounter ts
          refDate = tsReferenceDate ts


class (TermStructure ts) => YieldTermStructure ts where
  yieldDiscountT :: ts -> YearFrac -> DF

  yieldDiscount :: ts -> Day -> DF
  yieldDiscount ts day = yieldDiscountT ts t
    where t = tsTimeFromReference ts day

  yieldZeroRateT :: ts -> YearFrac -> Compounding -> InterestRate
  yieldZeroRateT ts t comp
    | t < dt = yieldZeroRateT ts dt comp
    | otherwise = InterestRate compound (tsDayCounter ts) comp
    where compound = Rate  (recip df)
          (DF df)  = yieldDiscountT ts t

  yieldZeroRate :: ts -> Day -> Compounding -> InterestRate
  yieldZeroRate ts d comp = yieldZeroRateT ts t comp
    where t = tsTimeFromReference ts d

  yieldForwardRateT :: ts -> YearFrac -> YearFrac -> Compounding -> InterestRate
  yieldForwardRateT ts t1 t2 comp = impliedRate compound dc comp (t2' - t1')
    where dc = tsDayCounter ts
          (t1', t2') = if t1 == t2 then
                         (max (t1 - dt/2.0) 0, t1 + dt)
                       else
                         (t1, t2)
          compound = coerce (yieldDiscountT ts t1' / yieldDiscountT ts t2')

  yieldForwardRate :: ts -> Day -> Day -> Compounding -> InterestRate
  yieldForwardRate ts d1 d2 comp= yieldForwardRateT ts t1 t2 comp
    where t1 = tsTimeFromReference ts d1
          t2 = tsTimeFromReference ts d2

dt :: YearFrac
dt = YearFrac 0.0001




