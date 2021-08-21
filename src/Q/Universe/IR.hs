module Q.Universe.IR where
import           Control.Lens.TH
import           Data.Time (Day)
import qualified Q.Correlation as Correl
import           Q.Currency
import           Q.Options.ImpliedVol.VolCube (VolCube)
import qualified Q.TermStructures.Yield.DiscountCurve as DiscountCurve
import           Q.Time.Date

data IR = IR
  {
    _refDate       :: Day
  , _calendar      :: Calendar
  , _currency      :: Currency
  , _oisIndexName  :: String
  , _ois           :: DiscountCurve.Interpolated
  , _iborIndexName :: String
  , _ibor1M        :: DiscountCurve.Interpolated
  , _ibor3M        :: DiscountCurve.Interpolated
  , _ibor6M        :: DiscountCurve.Interpolated
  , _ibor12M       :: DiscountCurve.Interpolated
  , _volCube       :: VolCube
  , _cmsCorrelCube :: Correl.Cube
  }

makeLenses ''IR
