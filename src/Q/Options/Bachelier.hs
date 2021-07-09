{-# LANGUAGE MultiParamTypeClasses #-}

module Q.Options.Bachelier (
    Bachelier(..)
  , euOption
  , eucall
  , euput
  , module Q.Options
) where
import           Data.Time                      ()
import           Q.Stochastic.Discretize        ()
import           Q.Stochastic.Process           ()
import           Q.Time                         ()
import           Statistics.Distribution        (cumulative, density)
import           Statistics.Distribution.Normal (standard)
import           Control.Monad.State
import           Data.Random                    (RVar, stdNormal)
import           Q.MonteCarlo
import           Q.Options
import           Q.Types


data Bachelier = Bachelier Forward Rate Vol deriving Show

-- | European option valuation with bachelier model.
euOption ::  Bachelier -> YearFrac -> OptionType -> Strike -> Valuation
euOption (Bachelier (Forward f) (Rate r) (Vol sigma)) (YearFrac t) cp (Strike k)
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
euput b t =  euOption b t Put

-- | see 'euOption'
eucall b t = euOption b t Call


instance Model Bachelier Double where
  discountFactor (Bachelier _ r _) t1 t2 = return $ exp (scale dt r)
    where dt = t2 - t1

  evolve (Bachelier (Forward f) (Rate r) (Vol sigma)) (YearFrac t) = do
    (YearFrac t0, f0) <- get
    let dt = t - t0
    dW <- (lift stdNormal)::StateT (YearFrac, Double) RVar Double
    let ft = f0 * exp (r * dt) + sqrt(sigma*sigma/2*r * ((exp (2 * r * dt)) - 1)) * dW
    put (YearFrac t, ft)
    return ft
