{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Q.Universe.EQ
where

import           Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time (Day, addDays, fromGregorian)
-- -- import qualified Data.Vector as V
import           Control.Lens (makeLenses, (^.))
import           Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift), liftEither, runExceptT)
import qualified Data.ByteString.Lazy as B
import           Data.Coerce (coerce)
import           Data.List (zip4)
import           Data.Ord (Down (Down))
import qualified Data.SortedList as SortedList
import           Data.Time.LocalTime (localDay)
import           GHC.Generics (Generic)
import           Q.Currencies (fromCode)
import           Q.Currency (Currency)
import           Q.Options.ImpliedVol (BSSurface (BSTermSurface), VolType (LogNormal), volKT)
import           Q.Options.ImpliedVol.InterpolatingSmile (InterpolatingSmile (..))
import           Q.Options.ImpliedVol.StrikeInterpolation (ExtrapolationMethod (Constant),
                                                           InterpolationMethod (Cubic),
                                                           mkInterpolator)
import           Q.Options.ImpliedVol.Surface (Surface (..), VolSurface (surfaceTotalVarKT))
import           Q.Options.ImpliedVol.TimeInterpolation (TimeInterpolation (LinearInTotalVar))
import qualified Q.SortedVector as SortedVector
import           Q.TermStructures (NoDiscounting (NoDiscounting),
                                   YieldTermStructure (yieldDiscountT))
import qualified Q.TermStructures.Yield.DiscountCurve as DiscountCurve
import           Q.Time.DayCounter (DayCounter (Act365_25), dcYearFraction)

import           Q.Universe.IR (DatesAndValues, IR, dDates, dValues, irFromFile, oisCurve)

import           Graphics.Gnuplot.Simple (plotList, Attribute (Title), plotLists)

import           Q.IR (Compounding (Continuous))
import qualified Q.TermStructures.Yield.ZeroCurve as ZeroCurve
import Q.Types





data DivStyle = Absolute
              | Proportional
              deriving stock (Enum, Generic, Read, Show)


instance FromJSON DivStyle

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

data EqAsset = EqAsset
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
  , _eqVolSurface :: BSSurface
  }
makeLenses ''EqAsset




data DBSSurface = DBSSurface
  {
    _dStrikesMatrix :: [[Strike]]
  , _dTenors        :: [Day]
  , _dVolMatrix     :: [[Vol]]
  } deriving stock (Generic, Show)

makeLenses ''DBSSurface


mkInterpolatingVolSurface :: Day -> (YearFrac -> Forward) -> DBSSurface -> Either String (Surface (InterpolatingSmile LogRelStrike) LogRelStrike)
mkInterpolatingVolSurface d fwdCurve surface = let ts = map (dcYearFraction Act365_25  d) (surface ^. dTenors)
                                                   fs = map fwdCurve ts
                                                   smiles = map mkInterpolatingSmile  (zip4 ts fs (surface ^. dStrikesMatrix) (surface ^.dVolMatrix))
                                               in return Surface
                                                  {
                                                    _surfaceSpot = coerce $ fwdCurve 0
                                                  , _surfaceTenors = SortedVector.fromList ts
                                                  , _surfaceForwardCurve = fwdCurve
                                                  , _surfaceSmiles = M.fromList (zip ts smiles)
                                                  , _surfaceTimeInterpolation  = LinearInTotalVar
                                                  , _surfaceType   = LogNormal
                                                  }


mkInterpolatingSmile ::  (YearFrac, Forward, [Strike] , [Vol]) -> InterpolatingSmile LogRelStrike
mkInterpolatingSmile (t, f, ks, vs) = StrikeSmile
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
  , _dEqTicker           :: T.Text
  , _dEqCurrency         :: T.Text
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


instance FromJSON Div where
  parseJSON = withObject "Div" $ \v -> Div
      <$> v .: "Amount"
      <*> (localDay <$> v .: "ExDate")
      <*> (localDay <$> v .: "PayDate")
      <*> v .: "DividendStyle"


instance FromJSON DEqAsset where
  parseJSON = withObject "DEqAsset" $ \v -> DEqAsset
      <$> (localDay <$> v .: "RefDate")
      <*> v .: "RIC"
      <*> ((v .: "Currency") >>= (.: "Shortcut"))
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



mkEQ :: IR -> DEqAsset -> Either String EqAsset
mkEQ ir dEq = do
  let d = dEq ^. dEqRefDate
      s = dEq ^. dEqSpot
      r = ir ^. oisCurve
      q = NoDiscounting
  b  <- mkYieldCurve d (dEq ^. dEqBorrowCost)
  dq <- boostrstrapDiscreteDivCurve d Act365_25 s r b q (dEq ^. dEqExpectedDiv) True
  let f includeDiv t =  Forward $ coerce s * qDF * dqDF  * bDF / rDF where
                        (DF qDF ) = yieldDiscountT q t
                        (DF dqDF) = if includeDiv then yieldDiscountT dq t else 1
                        (DF bDF ) = yieldDiscountT b t
                        (DF rDF ) = yieldDiscountT r t
  volSurface <- mkInterpolatingVolSurface d (f True) (dEq ^. dEqBsTermVolSurface)
  return EqAsset
    {  _eqRefDate   = d
    , _eqTicker     = Ticker . T.unpack $ (dEq ^. dEqTicker)
    , _eqCurrency   = fromCode (T.unpack (dEq ^. dEqCurrency))
    , _eqIsIndex    = dEq ^. dEqIsIndex
    , _eqSpot       = dEq ^. dEqSpot
    , _eqDivYield   = dq
    , _eqBorrowCost = b
    , _eqFwd        = f
    , _eqIR         = ir
    , _eqVolSurface = BSTermSurface (\k t -> totalVarToVol (surfaceTotalVarKT volSurface k t) t)
    }


irf = "/Users/ghaisissa/Downloads/ir.json"
eqf = "/Users/ghaisissa/Downloads/eq.json"

deqFromFile :: FilePath -> IO (Either String DEqAsset)
deqFromFile path = runExceptT $ do
  f <- lift $ B.readFile path
  liftEither $  eitherDecode f
eqFromFile :: FilePath -> FilePath -> IO (Either String EqAsset)
eqFromFile irPath jsonFile = runExceptT $ do
  ir <- ExceptT $ irFromFile irPath
  f <- lift $ B.readFile jsonFile
  dEQ <- liftEither $  eitherDecode f
  liftEither $ mkEQ ir dEQ


testeq = do
  eq <- eqFromFile "/Users/ghaisissa/Downloads/ir.json" "/Users/ghaisissa/Downloads/eq.json"
  case eq of
    Left str  -> print str
    Right peq -> testpeq peq

testpeq eq = do
  --print (eq ^. eqSpot)
  --print (eq ^. eqIsIndex)
  --plotCurve (eq ^. eqBorrowCost) "borrow cost"
  --plotCurve (eq ^. eqDivYield) "div yield"
  --plotCurve (eq ^. (eqIR . oisCurve)) "ois"
  --plotFwd ((eq ^. eqFwd) True)
  plotSurface (eq ^. eqVolSurface)
plotFwd :: (YearFrac -> Forward) -> IO ()
plotFwd f = do
  let ts  = map YearFrac [0, 0.01..10]
      fwds = map f ts
  mapM_ print fwds
  plotList [] (zip (coerce ts::[Double]) (coerce fwds::[Double]))



plotCurve c title = do
  let ts  = map YearFrac [0, 0.01..10]
      dfs = map (yieldDiscountT c) ts
  mapM_ print dfs
  plotList [Title title] (zip (coerce ts::[Double]) (coerce dfs::[Double]))


mkYieldCurve :: Day -> DatesAndValues -> Either String ZeroCurve.Interpolated
mkYieldCurve d curve =
  ZeroCurve.mkZeroCurve d Act365_25  (SortedList.toSortedList (zip (curve ^. dDates) (map Rate ( curve ^. dValues)))) Continuous



{-plotSurface surface = do
  let ts  = map (dcYearFraction Act365_25 (fromGregorian 2021 9 17)) [fromGregorian 2021 10 1, fromGregorian 2021 11 1, fromGregorian 2021 12 1, fromGregorian 2022 6 1]
      xs  = map LogRel [0]--[-1.6,-1.59..1.3]
      vols :: [[(Double, Double)]]
      f t = map (\x -> (coerce x, coerce (surfaceTotalVarKT surface x t))) xs
      vols = map f ts
--  print ts
  print vols
  plotLists [Title "Surface"] vols
-}
plotSurface :: BSSurface  ->  IO ()
plotSurface BSTermSurface{..} = do
  let ts  = map YearFrac [0.1, 0.6..10.5]

      f :: YearFrac -> [(Double, Double)]
      f t = map (\x -> (coerce x, coerce volToTotalVar (volKT x t) t)) strikes
      vols :: [[(Double, Double)]]
      vols = map f ts
    in plotLists [Title "Surface"] vols


strikes = map Strike [
  						886.538,
						1773.076,
						2216.345,
						2659.614,
						2881.2485,
						3102.883,
						3324.5175,
						3546.152,
						3767.7865,
						3989.421,
						4033.7479,
						4078.0748,
						4122.4017,
						4166.7286,
						4211.0555,
						4233.21895,
						4255.3824,
						4277.54585,
						4299.7093,
						4321.87275,
						4344.0362,
						4366.19965,
						4388.3631,
						4410.52655,
						4432.69,
						4454.85345,
						4477.0169,
						4499.18035,
						4521.3438,
						4543.50725,
						4565.6707,
						4587.83415,
						4609.9976,
						4632.16105,
						4654.3245,
						4698.6514,
						4742.9783,
						4787.3052,
						4831.6321,
						4875.959,
						5097.5935,
						5319.228,
						5540.8625,
						5762.497,
						5984.1315,
						6205.766,
						6649.035,
						7092.304,
						7535.573,
						7978.842,
						8422.111,
						8865.38,
						13298.07

                     ]
