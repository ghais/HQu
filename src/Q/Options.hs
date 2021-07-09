
module Q.Options (
    Valuation(..)
  , intrinsinc
  , hasTimeValue
  , module Q.Types) where

import           Numeric.IEEE
import           Q.Types


data Valuation = Valuation {
    vPremium :: Premium
  , vDelta   :: Delta
  , vVega    :: Vega
  , vGamma   :: Gamma
} deriving (Show)


-- | intrinsinc value of an option.
intrinsinc :: OptionType -> Forward -> Strike -> DF -> Double
intrinsinc Call (Forward f) (Strike k) (DF df) = max (f - k) 0
intrinsinc Put  (Forward f) (Strike k) (DF df) = max (k - f) 0

-- | returns True if the undiscounted option premium is greater than the 'intrinsinc'
hasTimeValue ::
     OptionType
  -> Forward
  -> Strike
  -> Premium
  -> DF
  -> Bool
hasTimeValue cp f k p df =  df `undiscount` p' - (intrinsinc cp f k df) > epsilon
    where (Premium p') = p

