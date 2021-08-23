{-# LANGUAGE ForeignFunctionInterface #-}
module Q.Options.ImpliedVol.LetsBeRational (
  euImpliedVol
) where

import           Data.Coerce (coerce)
import           Foreign.C.Types
import           Q.Types
import Q.Options.Black76 (B76Monad)





foreign import ccall
   "lets_be_rational.h implied_volatility_from_a_transformed_rational_guess" c_lbr ::
     CDouble -> CDouble  -> CDouble -> CDouble  -> CDouble  -> CDouble

-- | Calculate implied volatility for a European option using Let's Be Rational.
euImpliedVol :: OptionType -> Forward -> Strike -> YearFrac -> Rate -> Premium -> B76Monad Vol
euImpliedVol cp (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p) =
  return $ coerce $ c_lbr (CDouble undiscountP) (CDouble f) (CDouble k) (CDouble t) (CDouble (cpi cp))
  where undiscountP = undiscount  (discountFactor (coerce t) (coerce r)) p
