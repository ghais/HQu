{-# LANGUAGE MultiParamTypeClasses #-}

module Q.Options.Bachelier
  (
    Bachelier(..)
  , euOption
  , eucall
  , euput
  ) where

import           Control.Monad.State
import           Data.Random (RVar, stdNormal)
import           Q.MonteCarlo
import           Q.Options
import           Statistics.Distribution (cumulative)
import           Statistics.Distribution.Normal (standard)

data Bachelier = Bachelier YearFrac Forward Rate Vol deriving stock Show

-- | European option valuation with bachelier model.
euOption ::  Bachelier -> OptionType -> Strike -> Valuation
euOption (Bachelier (YearFrac t ) (Forward f) (Rate r) (Vol sigma)) cp (Strike k)
  = Valuation premium delta vega gamma where
    premium = Premium $ df * (q*(f - k)*n(q*d1) + sigma*sqrt(t)/sqrt2Pi * (exp(-0.5 *d1 * d1)))
    delta   = Delta   $ df * n (q * d1)
    vega    = Vega    $ df * (sqrt t) / sqrt2Pi * (exp (-0.5 * d1 * d1))
    gamma   = Gamma   $ (df/(sigma * (sqrt t)))*(recip sqrt2Pi)*(exp(-0.5 *d1 * d1))
    d1 = (f - k) / (sigma * sqrt(t))
    q = cpi cp
    sqrt2Pi = sqrt (2*pi)
    df =  exp $ (-r) * t
    n = cumulative standard

-- | see 'euOption'
euput :: Bachelier -> Strike -> Valuation
euput b =  euOption b Put

-- | see 'euOption'
eucall :: Bachelier -> Strike -> Valuation
eucall b = euOption b Call


instance Model Bachelier Bachelier where
  discountFactor (Bachelier _ _ r _) t1 t2 = return $ exp (scale dt r)
    where dt = t2 - t1

  evolve _ (YearFrac t) = do
    (YearFrac t0, (Bachelier _ (Forward f0) (Rate r) (Vol sigma))) <- get
    let dt = t - t0
    dW <- lift stdNormal::StateT (YearFrac, Bachelier) RVar Double
    let ft = f0 * exp (r * dt) + sqrt(sigma*sigma/2*r * (exp (2 * r * dt) - 1)) * dW
    put (YearFrac t, Bachelier (YearFrac t) (Forward ft) (Rate r) (Vol sigma))
    return ft

  
