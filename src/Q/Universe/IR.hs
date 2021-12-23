module Q.Universe.IR where
import           Control.Lens.TH (makeLenses)
import           Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time (Day)
import           Data.Time.LocalTime (LocalTime, localDay)
import           GHC.Generics (Generic)
import           Q.Currencies (fromCode)
import           Q.Currency (Currency)
import qualified Q.TermStructures.Yield.DiscountCurve as DiscountCurve
import           Q.Time.Date (Calendar (NullGregorian))

import           Control.Lens ((^.))
import           Data.SortedList (toSortedList)
import           Q.TermStructures.Yield.DiscountCurve (mkDiscountCurve)
import           Q.Time.DayCounter (DayCounter (Act365_25))
import           Q.Types (DF (DF), Strike (..))


data IR = IR
  {
    _refDate      :: Day
  , _currency     :: Currency
  , _calendar     :: Calendar
  , _oisCurve     :: DiscountCurve.Interpolated
  , _ibor1mCurve  :: DiscountCurve.Interpolated
  , _ibor3mCurve  :: DiscountCurve.Interpolated
  , _ibor6mCurve  :: DiscountCurve.Interpolated
  , _ibor12mCurve :: DiscountCurve.Interpolated
  }

makeLenses ''IR

data DatesAndValues = DatesAndValues
  {
    _dDates  :: [Day]
  , _dValues :: [Double]
  } deriving stock (Generic, Show)

makeLenses ''DatesAndValues

instance FromJSON DatesAndValues where
  parseJSON = withObject "DatesAndValues" $ \v -> DatesAndValues
    <$> (map localDay <$> v .: "Dates")
    <*> v .: "Values"

data DVolCube = DVolCube
  {
    _dSLNShift       :: Double
  , _dStrikeCube     :: [[[Strike]]]
  , _dFwdTicker      :: [T.Text]
  , _dTenorsMatrix   :: [[LocalTime]]
  , _dVolatilityCube :: [[[Double]]]
  } deriving stock (Generic, Show)

instance FromJSON DVolCube where
  parseJSON = withObject "DVolCube" $ \v -> DVolCube
    <$> v .: "SLNShift"
    <*> ((fmap . fmap . fmap) Strike <$> v .: "StrikesCube")
    <*> v .: "FwdTickers"
    <*> v .: "TenorsMatrix"
    <*> v .: "VolatilityCube"

type DCMSCorrelCub = M.Map String DatesAndValues

data DIR = DIR
  {
    _dRefDate  :: Day
  , _dCurrency :: T.Text
  , _dOIS      :: DatesAndValues
  , _dIBOR1M   :: DatesAndValues
  , _dIBOR3M   :: DatesAndValues
  , _dIBOR6M   :: DatesAndValues
  , _dIBOR12M  :: DatesAndValues
  } deriving stock (Generic, Show)

makeLenses ''DIR

instance FromJSON DIR where
  parseJSON = withObject "DIR" $ \v -> DIR
    <$> (localDay <$> v .: "RefDate")
    <*> ((v .: "Currency") >>= (.: "Shortcut"))
    <*> v .: "OIS"
    <*> v .: "IBOR_1M"
    <*> v .: "IBOR_3M"
    <*> v .: "IBOR_6M"
    <*> v .: "IBOR_12M"


mkIR :: DIR -> Either String IR
mkIR DIR{..} = do
  let d   = _dRefDate
      cny = fromCode (T.unpack _dCurrency)
  ois     <- mkInterpolatedCurve d _dOIS
  ibor1m  <- mkInterpolatedCurve d _dIBOR1M
  ibor3m  <- mkInterpolatedCurve d _dIBOR3M
  ibor6m  <- mkInterpolatedCurve d _dIBOR6M
  ibor12m <- mkInterpolatedCurve d _dIBOR12M
  return $ IR d cny NullGregorian ois ibor1m ibor3m ibor6m ibor12m


mkInterpolatedCurve :: Day -> DatesAndValues -> Either String DiscountCurve.Interpolated
mkInterpolatedCurve d curve =
  mkDiscountCurve d Act365_25 (toSortedList (zip (curve ^. dDates) (map DF ( curve ^. dValues))))

