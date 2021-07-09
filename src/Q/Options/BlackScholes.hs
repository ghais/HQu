{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Q.Options.BlackScholes (
    BlackScholes(..)
  , atmf
  , euOption
  , eucall
  , euput
  , module Q.Options
) where

import           Control.Monad.State
import           Data.Random                    hiding (Gamma)
import           Data.Time
import           Numeric.RootFinding
import           Q.ContingentClaim.Options
import           Q.MonteCarlo
import           Q.Options
import           Q.Stochastic.Discretize
import           Q.Stochastic.Process
import           Q.Time
import           Q.Types
import           Statistics.Distribution        (cumulative, density)
import           Statistics.Distribution.Normal (standard)
import qualified Q.Options.Black76 as B76

dcf = dcYearFraction ThirtyUSA

-- | Parameters for a simplified black scholes equation.
data BlackScholes = BlackScholes {
    bsSpot :: Spot -- ^ The asset's spot on the valuation date.
  , bsRate :: Rate   -- ^ Risk free rate.
  , bsVol  :: Vol    -- ^ Volatility.
} deriving Show



instance Model BlackScholes Double where
  discountFactor BlackScholes{..} t1 t2 = return $ exp (scale dt bsRate)
    where dt = t2 - t1

  evolve (BlackScholes spot (Rate r) (Vol sigma)) (YearFrac t) = do
    (YearFrac t0, s0) <- get
    let dt = t - t0
    dw <- (lift stdNormal)::StateT (YearFrac, Double) RVar Double
    let st = s0 * exp ((r - 0.5 * sigma * sigma) * dt + sigma * dw * sqrt dt)
    put (YearFrac t, st)
    return st

atmf :: BlackScholes -> YearFrac -> Strike
atmf BlackScholes{..} t = Strike $ s / d where
  (Rate d) = exp (scale t (-bsRate))
  (Spot s) = bsSpot



-- | European option valuation with black scholes.
euOption ::  BlackScholes ->  YearFrac -> OptionType -> Strike ->Valuation
euOption bs@BlackScholes{..} t cp k =
  let b76 = B76.Black76 {
          b76F  = forward bs t
        , b76DF = Q.Types.discountFactor t bsRate
        , b76T  = t
        , b76Vol = bsVol
        }
  in B76.euOption b76 cp k

-- | see 'euOption'
euput bs t = euOption bs t Put
 
-- | see 'euOption'
eucall bs t = euOption bs t Call

forward BlackScholes{..} (YearFrac t) = Forward $ s * exp (r * t)
  where (Spot s) = bsSpot
        (Rate r) = bsRate

corradoMillerIniitalGuess bs@BlackScholes{..} cp (Strike k) (YearFrac t) (Premium premium) =
  (recip $ sqrt t) * ((sqrt (2 * pi)/ (s + discountedStrike)) + (premium - (s - discountedStrike)/2) + sqrt ((premium - (s - discountedStrike)/2)**2 - ((s - discountedStrike)**2/pi))) where
    discountedStrike = k * (exp $ (-r) * t)
    (Rate r) = bsRate
    (Spot s) = bsSpot


