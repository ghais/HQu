module Q.IR where

import GHC.Generics (Generic)
import Q.Time.DayCounter (DayCounter)
import Q.Types
import GHC.Base (coerce)

data Compounding
  = Simple
  | Compounded Frequency
  | Continuous
  | SimpleThenCompound Frequency
  | CompoundedThenSimple Frequency
  deriving stock (Generic, Eq, Show, Read)

data Frequency
  = -- | once a year
    Annual
  | -- | twice a year
    Semiannual
  | -- | every fourth month
    EveryFourthMonth
  | -- | every third month
    Quarterly
  | -- | every second month
    Bimonthly
  | -- | once a month
    Monthly
  | -- | every fourth week
    EveryFourthWeek
  | -- | every second week
    Biweekly
  | -- | once a week
    Weekly
  | -- | once a day
    Daily
  | -- | some other unknown frequency
    OtherFrequency Int
  deriving stock (Generic, Eq, Show, Read)

freqNumEvents :: Frequency -> Double
freqNumEvents Annual = 1
freqNumEvents Semiannual = 2
freqNumEvents EveryFourthMonth = 3
freqNumEvents Quarterly = 4
freqNumEvents Bimonthly = 6
freqNumEvents Monthly = 12
freqNumEvents EveryFourthWeek = 13
freqNumEvents Biweekly = 26
freqNumEvents Weekly = 52
freqNumEvents Daily = 365
freqNumEvents (OtherFrequency n) = fromIntegral n

data InterestRate = InterestRate
  { irRate :: Rate,
    irDc :: DayCounter,
    irCompounding :: Compounding
  }
  deriving stock (Generic, Eq, Show, Read)

discountFactor :: InterestRate -> YearFrac -> DF
discountFactor ir t = coerce $ recip $ compoundFactor ir t

equivalentRate :: InterestRate -> Compounding -> YearFrac -> InterestRate
equivalentRate ir@InterestRate{..} comp t = impliedRate (compoundFactor ir t) irDc comp t

compoundFactor :: InterestRate -> YearFrac -> Double
compoundFactor (InterestRate (Rate r) _ Simple) (YearFrac t) = 1 + r * t
compoundFactor (InterestRate (Rate r) _ (Compounded freq)) (YearFrac t) = (1 + r / f) ** (f * t)
  where
    f = freqNumEvents freq
compoundFactor (InterestRate (Rate r) _ Continuous) (YearFrac t) = exp (r * t)
compoundFactor (InterestRate (Rate r) _ (SimpleThenCompound freq)) (YearFrac t) =
  if t < 1 / f
    then 1 + r * t
    else (1 + r / f) ** (f * t)
  where
    f = freqNumEvents freq
compoundFactor (InterestRate (Rate r) _ (CompoundedThenSimple freq)) (YearFrac t) =
  if t > 1 / f
    then 1 + r * t
    else (1 + r / f) ** (f * t)
  where
    f = freqNumEvents freq

impliedRate :: Double -> DayCounter -> Compounding -> YearFrac -> InterestRate
-- pattern matching on a double is odd, but this is a very special case that
-- corresponds to interest rate that is exactly 0.
impliedRate 1.0 dc compounding _ = InterestRate (Rate 0) dc compounding
impliedRate compound dc Simple (YearFrac t)  = InterestRate (Rate r) dc Simple
  where r = (compound - 1.0)/t
impliedRate compound dc (Compounded freq) (YearFrac t) = InterestRate (Rate r) dc (Compounded freq)
  where r = (compound**(1.0/f*t)-1)**f
        f = freqNumEvents freq
impliedRate compound dc Continuous (YearFrac t) = InterestRate (Rate r) dc Continuous
  where r = log compound / t
impliedRate compound dc (SimpleThenCompound freq) (YearFrac t)
  | t <= 1.0/f = impliedRate compound dc Simple (coerce t)
  | otherwise   = impliedRate compound dc Continuous (coerce t)
  where f = freqNumEvents freq
impliedRate compound dc (CompoundedThenSimple freq) (YearFrac t)
  | t > 1.0/f = impliedRate compound dc Simple (coerce t)
  | otherwise   = impliedRate compound dc Continuous (coerce t)
  where f = freqNumEvents freq


