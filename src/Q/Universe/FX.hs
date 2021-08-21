module Q.Universe.FX where


import qualified Q.TermStructures.Interpolated as TS

import           Control.Lens.TH
import           Q.Time.Date

import           Data.Time (Day)
import           Q.Currency (Currency)
import           Q.Options.ImpliedVol (BSSurface)
import           Q.Types

data Div = Absolute Double
         | Proportional Double
         | Yield Double
         | NoDiv

data FX = FX
  {
    _refDate          :: Day
  , _baseCurrency     :: Currency
  , _termCurrency     :: Currency
  , _baseCalendar     :: Calendar
  , _termCalendar     :: Calendar
  , _isIndex          :: Bool
  , _spot             :: Spot
  , _forwardCurve     :: TS.Interpolated Double
  , _bsTermVolSurface :: BSSurface
  }

makeLenses ''FX
