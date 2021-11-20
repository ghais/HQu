module Q.Stochastic.DupireLocalVol
  where

import Q.Types

import qualified Q.Options.ImpliedVol.Surface as IV
import Q.TermStructures hiding (dt, dt)
import Data.Coerce (coerce)



import Q.Stochastic.Process hiding (drift)
import Control.Monad.State.Strict (MonadState(get), put)
data DupireLocalVol r vs = LocalVol
  {
    _x      :: LogRelStrike
  , _s_0    :: Spot
  , _drift  :: r
  , _vs     :: vs
  , _eps_dw :: LogRelStrike
  , _eps_dt :: YearFrac
  }


instance (YieldTermStructure r, IV.VolSurface vs LogRelStrike) => YieldTermStructure (DupireLocalVol r vs) where
  yieldDiscountT LocalVol{..} = yieldDiscountT _drift

dupireLocalVariance :: (IV.VolSurface vs LogRelStrike, YieldTermStructure r) => DupireLocalVol r vs -> LogRelStrike -> YearFrac -> TotalVar
dupireLocalVariance lv@LocalVol{..} x t =
  if t < 1e-12 then
    dupireLocalVariance lv x 1e-12
  else
    let (g, dts) = derivatives
    in TotalVar $ abs (dts/g)
  where derivatives =
          let (TotalVar w)   = IV.surfaceTotalVarKT _vs x t
              (TotalVar w_l) = IV.surfaceTotalVarKT _vs (x - _eps_dw) t
              (TotalVar w_r) = IV.surfaceTotalVarKT _vs (x + _eps_dw) t
              dw  = (w_r - w_l) / (2 * coerce _eps_dw)
              dw2 = (w_r - 2 * w + w_l) / coerce (_eps_dw**2)
              gfunc = 1 - (coerce x / w) * dw + 0.5 * dw2 + 0.25 * (-0.25 - 1 / w + (coerce x / w)**2) * dw**2::Double
              (TotalVar w_p) = IV.surfaceTotalVarKT _vs x (t + _eps_dt)
              dT =  w_p / coerce _eps_dt
          in (gfunc, dT)



evolveDupire :: (IV.VolSurface vs LogRelStrike,  YieldTermStructure r) => (YearFrac, DupireLocalVol r vs) -> YearFrac -> Double -> DupireLocalVol r vs
evolveDupire (t, lv@LocalVol{..}) dt dW = lv{_x = s'}
  where (YearFrac t') = t + dt
        vol =  coerce $ sqrt (dupireLocalVariance lv  _x (coerce t))
        (Rate drift) = yieldContinuousZeroRateT lv t
        s' = LogRel $ (drift - 0.5 * vol * vol) * t' + vol * dW * sqrt t'


instance (IV.VolSurface vs LogRelStrike,  YieldTermStructure r) => StochasticProcess (DupireLocalVol r vs) Double where
  pEvolve_dt dt dw = do
    (t, lv) <- get
    let lv' = evolveDupire (t, lv) dt dw
    put (t + dt, lv')

