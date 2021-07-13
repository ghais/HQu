module Q.Options.ImpliedVol.TimeSlice
  (
      TimeSlice(..)
    , hasButterFlyArb
  )
where

import Numeric.GSL.Differentiation
import Q.Types

class TimeSlice v k where
  -- | we denote the the Black-Scholes implied volatility \( \sigma_{BS}(k, t) \) and define
  -- the total implied variance by \( \sigma_{BS}^2(k, t)t\)
  totalVar :: v -> k -> TotalVar

  -- | First derivative of 'totalVariance' with respect to strike
  dW :: v -> k -> Double

  -- | Second derivative of 'totalVariance' with respect to strike
  d2W :: v -> k -> Double

  -- | The implied density that is implied by the smile.
  --
  -- Note that the implied density is in the space of the strike k.
  impliedDensity :: v -> k -> Double


instance TimeSlice (LogRelStrike -> TotalVar) LogRelStrike  where
  totalVar f         = f
  dW f (LogRel k)    = fst $ derivCentral 1e-6 (\ k' -> let (TotalVar w ) = totalVar f (LogRel k') in w) k
  d2W f (LogRel k)   = let w' k' = dW f (LogRel k')
                       in fst $ derivCentral 1e-6 w' k
  impliedDensity f k = gk / sqrt(2*pi * wk) * exp (-0.5 * (dMinus**2))
    where (TotalVar wk) = totalVar f k
          sqrtWk = sqrt wk
          dMinus = -logK_F / sqrtWk - 0.5 * sqrtWk
          (LogRel logK_F) = k
          gk = let (TotalVar w)    = totalVar f k
                   w'              = dW f k
                   w''             = d2W f k
               in (1 - 0.5*logK_F*w' / w)**2 - 0.25*w'**2*(1/w + 0.25) + 0.5*w''

hasButterFlyArb :: (TimeSlice v k) => v -> [k] -> Tolerance -> Bool
hasButterFlyArb v ks (Tolerance eps) = any (\ k -> impliedDensity v k < eps) ks


