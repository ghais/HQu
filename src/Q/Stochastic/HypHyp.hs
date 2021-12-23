module Q.Stochastic.HypHyp
  (
    HypHyp(..)
  , exampleSLV
  ) where


import           Control.Monad.State.Strict (MonadState (put))
import           Data.Coerce (coerce)
import           Q.Stochastic.Process (ProcessDrift, SLVProcess (..), StochasticProcess (pEvolve'),
                                       constantDrift, correlated, slvLogDriftSvt, stateVariables,
                                       time)
import           Q.Types (Rho (Rho), Spot (..), Var (..), YearFrac (YearFrac))



data HypHyp = HypHyp 
              {
                _drift :: {-# UNPACK #-} !ProcessDrift
              , _s0    :: {-# UNPACK #-} !Double
              , _beta  :: {-# UNPACK #-} !Double
              , _alpha :: {-# UNPACK #-} !Double
              , _kappa :: {-# UNPACK #-} !Double
              , _rho   :: {-# UNPACK #-} !Double
              , _sigma :: {-# UNPACK #-} !Double
              }



fFunc :: HypHyp -> Double -> Double
fFunc HypHyp{..} x = let b = _beta :: Double
                         b2 = b*b
                 in ((1 - b + b2) * x + (b - 1) * (sqrt(x * x + b2 * ((1-x)**2)) - b))/b

instance SLVProcess HypHyp where
  slvDrift HypHyp{..}                   = _drift
  slvLeverage h@HypHyp{..} s _ _        = let x = coerce s/_s0 in (_sigma * fFunc h x)/x
  slvGamma _ (Var v)                    = v + sqrt(v * v + 1);
  slvCorrelate HypHyp{..} z1z2          =  correlated (coerce _rho) z1z2
  slvVarianceDrift HypHyp{..} (Var var) =  (-_kappa) * var
  slvVarianceVol HypHyp{..} _           = _alpha * sqrt(2 *  _kappa)


instance {-# OVERLAPPING #-} StochasticProcess HypHyp (Spot, Var) (Double, Double) where
  pEvolve' p@HypHyp{..} dZ1dZ2 dt = do
    (s0, v0)      <- stateVariables
    t0            <- time
    let (dZ1, dZ2) =  slvCorrelate p dZ1dZ2
        vol        = slvLeverage p s0 t0 dt * slvGamma p v0
        u          = slvLogDriftSvt p s0 v0 t0 dt * coerce dt
        s'         = Spot $ coerce s0 * exp (u + vol * sqrt(coerce dt) * dZ1)
        v'         = Var $ coerce v0  * exp((-_kappa) * coerce dt) + _alpha * sqrt(1 - exp((-2) * _kappa * coerce dt)) * dZ2;
    put (t0 + dt, (s', v'))
    return s'

newtype Sequence s = Sequence [s]


exampleSLV r s = HypHyp {
                         _drift= constantDrift r
                        , _s0= s
                        , _beta= 0.66
                        , _alpha= 0.55
                        , _kappa=2
                        , _rho= -0.37
                        , _sigma=0.2
                        }
