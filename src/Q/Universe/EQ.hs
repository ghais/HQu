module Q.Universe.EQ where

import qualified Q.TermStructures.Interpolated as TS
import qualified Q.TermStructures.Yield.ZeroCurve as ZeroCurve

import           Control.Lens.TH
import qualified Data.Map as M
import           Data.Time (Day)
import           Q.Currency (Currency)
import           Q.Options.ImpliedVol (BSSurface)
import           Q.Time.Date
import           Q.Types

data Div = Absolute Double
         | Proportional Double
         | Yield Double
         | NoDiv

data EQ = EQ
  {
    _refDate          :: Day
  , _calendar         :: Calendar
  , _currency         :: Currency
  , _isin             :: ISIN
  , _cusip            :: CUSIP
  , _isIndex          :: Bool
  , _spot             :: Spot
  , _divYield         :: ZeroCurve.Interpolated
  , _historicalDiv    :: M.Map Day Div
  , _burrowCost       :: ZeroCurve.Interpolated
  , _varswapSpread    :: TS.Interpolated Double
  , _bsTermVolSurface :: BSSurface
  }

makeLenses ''EQ
