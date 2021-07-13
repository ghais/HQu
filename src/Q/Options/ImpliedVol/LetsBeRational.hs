{-# LANGUAGE ForeignFunctionInterface #-}
module Q.Options.ImpliedVol.LetsBeRational (
  euImpliedVol
) where

import           Data.Coerce (coerce)
import           Foreign.C.Types
import           Q.Types


foreign import ccall
   "lets_be_rational.h implied_volatility_from_a_transformed_rational_guess" c_lbr ::
     CDouble -> CDouble  -> CDouble -> CDouble  -> CDouble  -> CDouble

-- | Calculate implied volatility for a European option using Let's Be Rational.
euImpliedVol :: OptionType -> Forward -> Strike -> YearFrac -> Rate -> Premium -> Vol
euImpliedVol cp (Forward f) (Strike k) (YearFrac t) (Rate r) (Premium p) =
  coerce $ c_lbr (CDouble p) (CDouble f) (CDouble k) (CDouble t) (CDouble (cpi cp))
