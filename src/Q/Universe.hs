module Q.Universe where
import           Control.Lens

import           Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import qualified Data.Map as M
import           Data.Time
import           GHC.Generics (Generic)
import           Q.Currencies (fromCode)
import           Q.Currency (Currency)
import qualified Q.Universe.EQ as EQ
import qualified Q.Universe.FX as FX
import qualified Q.Universe.IR as IR
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import           Q.Options.ImpliedVol.TimeSlice
import           Q.Types




newtype Asset = Asset String deriving newtype (Eq, Show, Read, Ord)

data Universe v = Universe
  {
    _refDate  :: Day
  , _irAssets :: M.Map Currency IR.IR
  , _eqAssets :: M.Map Ticker (EQ.EqAsset v)
  , _fxAssets :: M.Map (Currency, Currency) FX.FX
  }


data DUniverse = DUniverse
  {
    _dRefDate  :: Day
  , _dEquities :: [EQ.DEqAsset]
  , _dIR       :: [IR.DIR]
  } deriving stock (Generic, Show)

makeLenses ''DUniverse

instance FromJSON DUniverse where
  parseJSON = withObject "DUniverse" $ \v -> DUniverse
    <$> (localDay <$> v .: "RefDate")
    <*> v .: "Equities"
    <*> v .: "InterestRates"

makeLenses ''Universe


mkUniverse :: (EQ.EqSmile v) => DUniverse -> Either String (Universe v)
mkUniverse du = do
  irs <- mapM IR.mkIR (du ^. dIR)
  let irM = M.fromList (map (\ir -> (ir ^. IR.currency, ir)) irs)
  eqs <- mapM (\deq -> EQ.mkEQ (irM M.! fromCode (deq ^. EQ.dEqCurrency)) deq) (du ^. dEquities)
  let eqM = M.fromList (map (\eq -> (eq ^. EQ.eqTicker , eq)) eqs)
  return Universe
    {
      _refDate  = du ^. dRefDate
    , _irAssets = irM
    , _eqAssets = eqM
    , _fxAssets = M.empty
    }

irAsset :: Currency -> Universe v -> Maybe IR.IR
irAsset cny u = M.lookup cny (view irAssets u)

fxAsset :: Currency -> Currency -> Universe v -> Maybe FX.FX
fxAsset cny1 cny2 u | cny1 < cny2 = M.lookup (cny1, cny2) (view fxAssets u)
                    | otherwise   = M.lookup (cny2, cny1) (view fxAssets u)

eqAsset :: (TimeSlice v LogRelStrike ) => Ticker -> Universe v -> Maybe (EQ.EqAsset v)
eqAsset ticker u =  u ^. (eqAssets . at ticker)


duFromFile :: FilePath  -> IO (Either String DUniverse)
duFromFile path = runExceptT $ do
  f <- lift $ B.readFile path
  liftEither $  eitherDecode f


duOrError path = do
  u <- duFromFile path
  case u of
    Left err -> error (show err)
    Right du -> return du

puFromFile :: (EQ.EqSmile v) => FilePath  -> IO (Either String (Universe v))
puFromFile path = runExceptT $ do
  f <- lift $ B.readFile path
  deq <- liftEither $  eitherDecode f
  liftEither $ mkUniverse deq


puOrError p = do
  u <- puFromFile p
  case u of
    Left err -> error (show err)
    Right pu -> return pu
