{-# LANGUAGE UndecidableInstances #-}
module Q.Stochastic.DupireLocalVol
  where

import           Q.Types

import           Data.Coerce (coerce)
import qualified Q.Options.ImpliedVol.Surface as IV



import           Control.Lens (makeLenses, (^.))
import           Q.Stochastic.Process hiding (drift)
import           Q.TermStructures (ForwardCurveTermStructure (tsForwardT))


data DupireLocalVol vs f = Dupire
                           {
                             _drift      :: ProcessDrift
                           , _volSurface :: vs
                           , _fwdCurve   :: f
                           }

makeLenses ''DupireLocalVol

instance (IV.VolSurface v LogRelStrike, ForwardCurveTermStructure f) => LocalVol (DupireLocalVol v f)  where
  lvDrift lv = lv ^. drift

  lvLogSigmaSvt lv s t = let vol = sqrt (localVariance lv s t)
                             minVol = 1e-2
                             maxVol = 5
                         in max minVol  (min maxVol vol)

localVariance :: (IV.VolSurface v LogRelStrike, ForwardCurveTermStructure f) => DupireLocalVol v f -> Spot -> YearFrac -> Double
localVariance lv@Dupire{..} s t =
  if t < 1e-12 then
    localVariance lv s 1e-12
  else
    let (g, dts) = derivatives
    in abs (dts/g)
  where x           = logRelStrike (tsForwardT (lv ^. fwdCurve ) t) (coerce s)
        derivatives =
          let (TotalVar w)   = IV.surfaceTotalVarKT _volSurface x t
              (TotalVar w_l) = IV.surfaceTotalVarKT _volSurface (x - epsDw) t
              (TotalVar w_r) = IV.surfaceTotalVarKT _volSurface (x + epsDw) t
              dw  = (w_r - w_l) / (2 * coerce epsDw)
              dw2 = (w_r - 2 * w + w_l) / coerce (epsDw**2)
              gfunc = 1 - (coerce x / w) * dw + 0.5 * dw2 + 0.25 * (-0.25 - 1 / w + (coerce x / w)**2) * dw**2::Double
              (TotalVar w_p) = IV.surfaceTotalVarKT _volSurface x (t + epsDt)
              dT =  (w_p -w) / coerce epsDt
          in (gfunc, dT)
        epsDw = 1e-05
        epsDt = 1e-05






