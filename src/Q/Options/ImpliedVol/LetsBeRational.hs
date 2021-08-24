{-# LANGUAGE ForeignFunctionInterface #-}
module Q.Options.ImpliedVol.LetsBeRational (
  euImpliedVol
) where

import           Data.Coerce (coerce)
import Q.Options.Black76 (B76Monad)
import Q.Types
import LetsBeRational

-- | Calculate implied volatility for a European option using Let's Be Rational.
euImpliedVol :: OptionType -> Forward -> Strike -> YearFrac -> Rate -> Premium -> B76Monad Vol
euImpliedVol cp (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p) =
  return $ coerce $ lbr (cpi cp) f k t undiscountedP
  where undiscountedP = undiscount  (discountFactor (coerce t) (coerce r)) p
