{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Q.Universe.EQ
where

import           Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:), ToJSON (toJSON), object, (.=))
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time (Day, addDays, fromGregorian)
-- -- import qualified Data.Vector as V
import           Control.Lens (makeLenses, (^.))


import           Data.Coerce (coerce)
import           Data.List (zip4)
import           Data.Ord (Down (Down))
import qualified Data.SortedList as SortedList
import           Data.Time.LocalTime (localDay)
import           GHC.Generics (Generic)
import           Q.Currencies (fromCode)
import           Q.Currency (Currency)
import           Q.Options.ImpliedVol (BSSurface (BSTermSurface), VolType (LogNormal), volKT)
import           Q.Options.ImpliedVol.InterpolatingSmile (InterpolatingSmile (..), fitSVI)
import           Q.Options.ImpliedVol.StrikeInterpolation (ExtrapolationMethod (Constant),
                                                           InterpolationMethod (Cubic),
                                                           mkInterpolator)
import           Q.Options.ImpliedVol.Surface (Surface (..), VolSurface (surfaceTotalVarKT))
import           Q.Options.ImpliedVol.TimeInterpolation (TimeInterpolation (LinearInTotalVar))
import qualified Q.SortedVector as SortedVector
import           Q.TermStructures (NoDiscounting (NoDiscounting),
                                   YieldTermStructure (yieldDiscountT), ForwardCurveTermStructure (tsForwardT))
import qualified Q.TermStructures.Yield.DiscountCurve as DiscountCurve
import           Q.Time.DayCounter (DayCounter (Act365_25), dcYearFraction)

import           Q.Universe.IR (DatesAndValues, IR, dDates, dValues, oisCurve)

import           Data.List (find)

import           Q.IR (Compounding (Continuous))
import           Q.Options.ImpliedVol.SVI (SVI)
import           Q.Options.ImpliedVol.TimeSlice (TimeSlice, totalVar)
import qualified Q.TermStructures.Yield.ZeroCurve as ZeroCurve
import Q.Types
    ( Ticker(..),
      Vol(..),
      DF(..),
      Rate(Rate),
      YearFrac,
      LogRelStrike(..),
      Strike(..),
      Forward(..),
      Spot(..),
      logRelStrike,
      totalVarToVol )




class (TimeSlice smile LogRelStrike) => EqSmile smile where
  mkSmile :: (YearFrac, Forward, [Strike] , [Vol]) -> Either String smile



data DivStyle = Absolute
              | Proportional
              deriving stock (Enum, Generic, Read, Show)


instance FromJSON DivStyle
instance ToJSON DivStyle

data Div = Div
  {
    _divAmount  :: Double
  , _divExDate  :: Day
  , _divPayDate :: Day
  , _divStyle   :: DivStyle
  } deriving  stock (Generic, Show)

makeLenses ''Div

cashDiv :: Div -> Spot -> Double
cashDiv Div{..} (Spot s) = case _divStyle of
                             Absolute     -> _divAmount
                             Proportional -> s * _divAmount


instance Eq Div where
  a == b = a ^. divExDate == b ^. divExDate

instance Ord Div where
  a `compare` b = (a ^. divExDate) `compare` (b ^. divExDate)


type EqFwdCurve = Bool     -- ^ Include discrete dividends
                -> YearFrac -- ^ Time
                -> Forward  -- ^ Forward

data EqAsset v = EqAsset
  {
    _eqRefDate    :: Day
  , _eqTicker     :: Ticker
  , _eqCurrency   :: Currency
  , _eqIsIndex    :: Bool
  , _eqSpot       :: Spot
  , _eqDivYield   :: DiscountCurve.Interpolated
  , _eqBorrowCost :: ZeroCurve.Interpolated
  , _eqFwd        :: EqFwdCurve
  , _eqIR         :: IR
  , _eqVolSurface :: Surface v LogRelStrike
  }
makeLenses ''EqAsset


instance ForwardCurveTermStructure (EqAsset v) where
  tsForwardT eq t = (eq ^. eqFwd) False t

data DBSSurface = DBSSurface
  {
    _dStrikesMatrix :: [[Strike]]
  , _dTenors        :: [Day]
  , _dVolMatrix     :: [[Vol]]
  } deriving stock (Generic, Show)

makeLenses ''DBSSurface


getDSmile :: DBSSurface -> Day -> Maybe [(Strike, Vol)]
getDSmile surface refDate = do
 let zipped = zip3 (surface ^. dTenors) (surface ^. dStrikesMatrix) (surface ^. dVolMatrix)
 (_, ks, vs) <- find (\(d, _, _) -> d == refDate) zipped
 return $ zip ks vs


mkVolSurface :: (EqSmile v) => Day -> (YearFrac -> Forward) -> DBSSurface -> Either String (Surface v LogRelStrike)
mkVolSurface d fwdCurve surface = do
  let ts = map (dcYearFraction Act365_25  d) (surface ^. dTenors)
      fs = map fwdCurve ts
  smiles <- mapM mkSmile  (zip4 ts fs (surface ^. dStrikesMatrix) (surface ^.dVolMatrix))
  return Surface
    {
      _surfaceSpot = coerce $ fwdCurve 0
    , _surfaceTenors = SortedVector.fromList ts
    , _surfaceForwardCurve = fwdCurve
    , _surfaceSmiles = M.fromList (zip ts smiles)
    , _surfaceTimeInterpolation  = LinearInTotalVar
    , _surfaceType   = LogNormal
    }


mkSviSmile ::  (YearFrac, Forward, [Strike] , [Vol]) -> Either String SVI
mkSviSmile (t, f, ks, vs) = do
  smile <- mkInterpolatingSmile (t, f, ks, vs)
  fitSVI smile [logRelStrike f k | k <- ks]

mkInterpolatingSmile ::  (YearFrac, Forward, [Strike] , [Vol]) -> Either String (InterpolatingSmile LogRelStrike)
mkInterpolatingSmile (t, f, ks, vs) = return $ StrikeSmile
  {
    _forward = f
  , _tenor   = t
  , _vols    = mkInterpolator Cubic (SortedList.toSortedList (zip xs vs))
  , _extrapolation = Constant
  , _minStrike     = minimum xs
  , _maxStrike     = maximum xs
  }  where xs = map (logRelStrike f) ks


data DEqAsset = DEqAsset
  {
    _dEqRefDate          :: Day
  , _dEqTicker           :: String
  , _dEqCurrency         :: String
  , _dEqIsIndex          :: Bool
  , _dEqSpot             :: Spot
  , _dEqExpectedDiv      :: [Div]
  , _dEqBorrowCost       :: DatesAndValues
  , _dEqBsTermVolSurface :: DBSSurface
  } deriving stock (Generic,  Show)


makeLenses ''DEqAsset

instance FromJSON DBSSurface where
  parseJSON = withObject "DBSSurface" $ \v -> DBSSurface
    <$> ((fmap. fmap) Strike <$> v .: "StrikesMatrix")
    <*> (fmap localDay <$> v .: "Tenors")
    <*> ((fmap . fmap) Vol <$> v .: "VolatilityMatrix")

instance ToJSON DBSSurface where
  toJSON DBSSurface{..} = object ["StrikesMatrix" .= ((coerce _dStrikesMatrix)::[[Double]]), "Tenors" .= _dTenors, "" .= ((coerce _dVolMatrix)::[[Double]])]

instance FromJSON Div where
  parseJSON = withObject "Div" $ \v -> Div
      <$> v .: "Amount"
      <*> (localDay <$> v .: "ExDate")
      <*> (localDay <$> v .: "PayDate")
      <*> v .: "DividendStyle"


instance FromJSON DEqAsset where
  parseJSON = withObject "DEqAsset" $ \v -> DEqAsset
      <$> (localDay <$> v .: "RefDate")
      <*> (T.unpack <$> v .: "RIC")
      <*> ((v .: "Currency") >>= (.: "Shortcut") >>= (return . T.unpack))
      <*> v .: "IsIndex"
      <*> (Spot <$> v .: "Spot")
      <*> v .: "ExpectedDividends"
      <*> v .: "BorrowCost"
      <*> v .: "BSTermVolSurface"





boostrstrapDiscreteDivCurve :: (YieldTermStructure r, YieldTermStructure b, YieldTermStructure q) =>
  Day -> DayCounter -> Spot -> r -> b -> q -> [Div] -> Bool -> Either String DiscountCurve.Interpolated
boostrstrapDiscreteDivCurve d dc _ r _ _ []  _ = Right $ DiscountCurve.mkConstantDiscountCurve d dc (DF 1)


boostrstrapDiscreteDivCurve d dc s r b q divs allowExhaustFwd = DiscountCurve.mkRightContiniousDiscountCurve d dc dfs
  where f t = Forward $ coerce s  * coerce (yieldDiscountT b t * yieldDiscountT q t / yieldDiscountT r t)
        dfs = SortedList.toSortedList $ bootstrapDivs f [(d, 1)] divs allowExhaustFwd dc d

bootstrapDivs :: (YearFrac -> Forward) -> [(Day, DF)] -> [Div] -> Bool -> DayCounter  -> Day -> [(Day, DF)]
bootstrapDivs f divDFs (dividend:divs) allowExhaustFwd dc d
  = let exDate = dividend ^. divExDate
        exTime = dcYearFraction dc d exDate
        payDate = dividend ^. divPayDate
        payTime = dcYearFraction dc d payDate
        DF df  = (coerce . snd . head) divDFs
        fwd    = df * coerce (f exTime)
        cash   = if payTime > exTime then
                    cashDiv dividend (Spot fwd) * coerce (f exTime / f payTime)
                 else
                    cashDiv dividend (Spot fwd)
        df'    = DF $ coerce df * (1 - cash / fwd)
    in if allowExhaustFwd && df' <0 then
         bootstrapDivs f ((exDate, 1e-15):divDFs) divs allowExhaustFwd dc d
       else
         bootstrapDivs f ((exDate, df'):divDFs) divs allowExhaustFwd dc d
         --[(exDate, df')]
bootstrapDivs _ divDFs [] _ _ _ = reverse finalDivs where
  finalDivs = zip (addDays 365 lastD:days) divs
  (days, divs) = unzip divDFs
  lastD      = head days


lastElement :: SortedList.SortedList (a, b) -> (a, b)
lastElement xys = case SortedList.uncons (SortedList.reverse xys) of
                    Nothing                 -> error "Interpolation is empty"
                    (Just (Down (x, y), _)) -> (x, y)



mkEQ :: (EqSmile v) => IR -> DEqAsset -> Either String (EqAsset v)
mkEQ ir dEq = do
  let d = dEq ^. dEqRefDate
      s = dEq ^. dEqSpot
      r = ir  ^. oisCurve
      q = NoDiscounting
  b  <- mkYieldCurve d (dEq ^. dEqBorrowCost)
  dq <- boostrstrapDiscreteDivCurve d Act365_25 s r b q (dEq ^. dEqExpectedDiv) True
  let f includeDiv t =  Forward $ coerce s * qDF * dqDF  * bDF / rDF where
                        (DF qDF ) = yieldDiscountT q t
                        (DF dqDF) = if includeDiv then yieldDiscountT dq t else 1
                        (DF bDF ) = yieldDiscountT b t
                        (DF rDF ) = yieldDiscountT r t
  volSurface <- mkVolSurface d (f True) (dEq ^. dEqBsTermVolSurface)
  return EqAsset
    {
      _eqRefDate   = d
    , _eqTicker     = Ticker  (dEq ^. dEqTicker)
    , _eqCurrency   = fromCode (dEq ^. dEqCurrency)
    , _eqIsIndex    = dEq ^. dEqIsIndex
    , _eqSpot       = dEq ^. dEqSpot
    , _eqDivYield   = dq
    , _eqBorrowCost = b
    , _eqFwd        = f
    , _eqIR         = ir
    , _eqVolSurface = volSurface
    }

volKT :: (TimeSlice v LogRelStrike) => EqAsset v -> Strike -> YearFrac -> Vol
volKT asset k t = totalVarToVol t (surfaceTotalVarKT (asset ^. eqVolSurface) k t)

mkYieldCurve :: Day -> DatesAndValues -> Either String ZeroCurve.Interpolated
mkYieldCurve d curve =
  ZeroCurve.mkZeroCurve d Act365_25  (SortedList.toSortedList (zip (curve ^. dDates) (map Rate ( curve ^. dValues)))) Continuous




instance EqSmile SVI where
  mkSmile = mkSviSmile

instance EqSmile (InterpolatingSmile LogRelStrike) where
  mkSmile = mkInterpolatingSmile
