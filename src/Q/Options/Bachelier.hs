module Q.Options.Bachelier
  (
    Bachelier(..)
  , euOption
  , eucall
  , euput
  , theta1D
  ) where

import           Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT)
import           Data.Random (RVar, stdNormal)
import           Numeric.GSL.Differentiation (derivCentral)
import           Q.MonteCarlo (Model (..))
import           Q.Options 
import           Q.Options.ImpliedVol.TimeSlice (TimeSlice (..), ImpliedDensity(..))
import           Statistics.Distribution (cumulative)
import           Statistics.Distribution.Normal (standard)
import Control.Lens.Internal.Coerce (coerce)
import Q.Types

data Bachelier = Bachelier YearFrac Forward Rate Vol deriving stock Show

-- | European option valuation with bachelier model.
euOption ::  Bachelier -> OptionType -> Strike -> Valuation
euOption (Bachelier (YearFrac t ) (Forward f) (Rate r) (Vol sigma)) cp (Strike k)
  = Valuation premium delta vega gamma theta rho where
    premium    = Premium $ df * (q*(f - k)*n(q*d1) + sigma * sqrt(t) * (n' d1))
    delta      = Delta   $ df * n (q * d1)
    vega       = Vega    $ df * (sqrt t) * (n' d1)
    gamma      = Gamma   $ (df/(sigma * (sqrt t)))*(n' d1)
    thetaCall  = Theta   $ r * (coerce premium) - df * sigma * (n' d1) / (2 * sqrt t)
    thetaPut   = Theta   $ r * (coerce premium) - df * (sigma * (n' d1) / (2 * sqrt t) - 2 * (f - k))
    rho        = Rho     $ -t * (coerce premium)
    theta      = if cp == Call then thetaCall else thetaPut
    d1 = (f - k) / (sigma * sqrt(t))
    q = cpi cp
    sqrt2Pi = sqrt (2*pi)
    df =  exp $ (-r) * t
    n = cumulative standard
    n' d = (recip sqrt2Pi) * (exp(-0.5 * d * d))


theta1D :: Bachelier -> OptionType -> Strike -> Theta1D
theta1D b cp k = coerce $ vPremium (euOption (decayOneDay b) cp k) - vPremium (euOption b cp k)

-- | see 'euOption'

decayOneDay :: Bachelier -> Bachelier
decayOneDay (Bachelier t f r sigma) = if t < oneDay then
                                        Bachelier (YearFrac 0.0001) f r sigma
                                      else
                                        Bachelier (t - oneDay) f r sigma
  where oneDay = 1/365
euput :: Bachelier -> Strike -> Valuation
euput b =  euOption b Put

-- | see 'euOption'
eucall :: Bachelier -> Strike -> Valuation
eucall b = euOption b Call


instance Model Bachelier Bachelier where
  discountFactor (Bachelier _ _ r _) t1 t2 = return $ exp (scale dt r)
    where dt = t2 - t1

  evolve _ (YearFrac t) = do
    (YearFrac t0, Bachelier _ (Forward f0) (Rate r) (Vol sigma)) <- get
    let dt = t - t0
    dW <- lift stdNormal::StateT (YearFrac, Bachelier) RVar Double
    let ft = f0 * exp (r * dt) + sqrt(sigma*sigma/2*r * (exp (2 * r * dt) - 1)) * dW
    put (YearFrac t, Bachelier (YearFrac t) (Forward ft) (Rate r) (Vol sigma))
    return ft


instance TimeSlice Bachelier Strike where
  totalVar (Bachelier t _ _ vol) _ = volToTotalVar vol t

instance ImpliedDensity Bachelier Strike where
  dW _ _ = 0
  d2W _ _ = 0
  impliedDensity bachelier (Strike k) = let
    dk k' = fst (derivCentral 1e-4 (\k2 -> let (Premium v) = vPremium (eucall bachelier (Strike k2)) in v) k')
    in fst (derivCentral 1e-4 dk k)
