module Q.Stochastic.CEV
  where
import Q.Types (Sigma(..),Beta(..), Spot(..))
import Q.Stochastic.Process (LocalVol(..), ProcessDrift, drift)

import Data.Coerce
import Control.Lens ((^.), makeLenses)

data CEV = CEV
  {
    _cevSigma :: Sigma
  , _cevBeta :: Beta
  , _cevDrift  :: ProcessDrift
  }

makeLenses ''CEV

instance LocalVol CEV where
  lvLogSigmaSvt cev (Spot s) _ = (coerce $ cev ^. cevSigma) *  (s**(coerce $ (cev ^. cevBeta) - 1))
  lvLogDriftSvtDt cev s t dt = let vol = (lvLogSigmaSvt cev s t)
                              in (drift (lvDrift cev) t dt) - 0.5 * vol * vol

  lvDrift :: CEV -> ProcessDrift 
  lvDrift cev = cev ^. cevDrift

