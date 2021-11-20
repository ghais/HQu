module Q.Options (
    Valuation(..)
  , Theta1D(..)
  , intrinsic
  , hasTimeValue
  ) where

import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           Numeric.IEEE
import           Q.Types


data Valuation = Valuation {
    vPremium :: Premium
  , vDelta   :: Delta
  , vVega    :: Vega
  , vGamma   :: Gamma
  , vTheta   :: Theta
  , vRho     :: Rho
} deriving stock (Show)


-- | intrinsinc value of an option.
intrinsic :: OptionType -> Forward -> Strike -> Double
intrinsic Call (Forward f) (Strike k) = max (f - k) 0
intrinsic Put  (Forward f) (Strike k) = max (k - f) 0

-- | returns True if the undiscounted option premium is greater than the 'intrinsinc'
hasTimeValue ::
     OptionType
  -> Forward
  -> Strike
  -> Premium
  -> DF
  -> Bool
hasTimeValue cp f k p df =  df `undiscount` p' - (intrinsic cp f k) > epsilon
    where (Premium p') = p


newtype Theta1D = Theta1D Double deriving stock (Generic, Eq, Show, Read, Ord)
                                 deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
