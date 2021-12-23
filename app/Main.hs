{-# LANGUAGE BangPatterns #-}
{- |
Copyright: (c) 2021 Ghais
SPDX-License-Identifier: MIT
Maintainer: Ghais <0x47@0x49.dev>

General purpose quantitative finance library
-}

module Main where
import Data.Word
import System.Random.MWC.Distributions hiding (beta)
import Data.Random
import Data.Random.Source.PureMT
import Control.Monad.State.Strict



import qualified Data.Vector.Unboxed as V
import Q.Options.ImpliedVol.TimeSlice
import Q.Options.ImpliedVol.SVI
import Graphics.Vega.VegaLite
    ( encoding,
      layer,
      mark,
      position,
      dataColumn,
      dataFromColumns,
      toHtmlFile,
      asSpec,
      toVegaLite,
      PositionChannel(PmType, PName),
      DataValues(Numbers),
      Measurement(Quantitative),
      Position(Y, X),
      Mark(Line),
      MarkProperty(MColor) )
import Q.Options.Black76
import Numeric.GSL.Integration
import           Graphics.Gnuplot.Simple

import Q.Types
    ( ($/$),
      volToTotalVar,
      DF(DF),
      Forward(Forward),
      Kappa(Kappa),
      Lambda(Lambda),
      LogRelStrike(..),
      Spot(Spot),
      Strike(Strike),
      Theta(Theta),
      TotalVar(TotalVar),
      Var(Var),
      Vol(Vol),
      YearFrac(YearFrac), Ticker (Ticker) )

import Q.Options.ImpliedVol.Surface (surfaceTotalVarKT, ConstVolSurface (ConstVolSurface))
import Data.Coerce
import Q.Stochastic.Process hiding (rho)

import Statistics.Sample




import Q.Stochastic.DupireLocalVol
import Q.TermStructures (FwdCurve(..))
import Q.TermStructures.Yield.DiscountCurve (ConstRate(ConstRate))

import Q.Universe (puFromFile, puOrError, eqAsset, Universe (Universe))


import Data.Maybe (fromJust)
import Q.Universe.EQ
import Control.Lens.Operators ((^.))
import Q.Stochastic.CEV (CEV(CEV))
import Q.Options.ImpliedVol.InterpolatingSmile (InterpolatingSmile)
import Q.Stochastic.HypHyp
import Data.Random.Source.MWC (create)
import Data.Random (Normal(StdNormal))

import Data.Random.Source.PureMT (pureMT)







alpha = Alpha 1
beta = Beta 0
rho = Rho 0
m = M 0
sigma = Sigma 0
ttm = YearFrac 1

jw = JWSVI ttm (V 1) (Phi 0.0) (P 0.0000) (C 0.00000) (VTil 1)
svi = RSVI ttm alpha beta rho m sigma

f = Forward 100
strikes = [LogRel (log (k $/$ f)) | k <- [1..2000]::[Double]]
--strikes :: [Double]
--strikes = [1..2000]

b76 = Black76 f (DF 1) (YearFrac 1) (Vol 1)

testOptionIntegration = do
  let truePrice = eucall b76 (Strike 100)
      integrationPrice = integrateQAGS 0.001 100 f  (-1) 3
      f x = max (100 * (exp x) - 100) 0 * impliedDensity svi (LogRel x)
  print truePrice
  print integrationPrice


heston = HestonModel (constantDrift 0.01) (Kappa 3) (Theta 0.02) (Sigma 0.4) (Rho (-0.6))
gbm = GBM 0.01 0.1
ts  = map YearFrac [0,0.005..1]


testProcess :: (Distribution d b, StochasticProcess p s b) => p -> s -> d b -> Word64 -> IO ()
testProcess p s d i= do
  let gbms = evalState (oneTrajectory p s d  ts) (pureMT i)
  plotList [] (zip (coerce (tail ts)::[Double]) (coerce gbms::[Double]))


testFwd :: (Distribution d b, StochasticProcess p s b) => p -> s -> d b -> IO ()
testFwd p s d = do
  let n = 128000::Int
      !finalSpots = evalState (trajectories'  n p s d  ts) (pureMT 0)
--      !finalSpots = map last gbms
  print $ sum finalSpots / fromIntegral n
  print $ stdDev (V.fromList ((coerce finalSpots)::[Double]))

testGBM :: IO ()
testGBM = testFwd gbm (Spot 100) (StdNormal::Normal Double)

testHeston :: IO ()
testHeston = testProcess heston (Spot 100, Var 0.4) t 0 where
  t :: UncorrelatedPair (Normal Double) (Double, Double)
  t = UncorrelatedPair StdNormal



myssvi = let vol = Vol 0.2
             rho  = Rho 0.3
             lambda = Lambda 0.9
         in SSVI (`volToTotalVar` vol) rho lambda


testDupire =
  let drift = constantDrift 0.05
      s     = Spot 100
      ssvi  = myssvi
      fwd   = YieldFwdCurve s (ConstRate 0.05)
      dupire = Dupire drift ssvi fwd
    in testFwd dupire (Spot 100) (StdNormal::Normal Double)


testCEV =
  let drift = constantDrift 0.05
      s     = Spot 100
      gamma = 1
      sigma = 0.25
      cev = CEV sigma gamma drift
    in testFwd cev (Spot 100) (StdNormal::Normal Double)
testSSVI = do
  let vol = Vol 0.2
      rho  = Rho 0.3
      lambda = Lambda 0.9
      ssvi = SSVI (`volToTotalVar` vol) rho lambda
      ts   = [0.05, 0.1, 0.2, 0.3, 0.5]::[Double]
      ks   = [-0.9, -0.89..1.2]::[Double]
  plotFunc3d [] [] ks ts (\k t -> coerce $ surfaceTotalVarKT ssvi (LogRel k) (YearFrac t)::Double)





someFunc :: IO ()
someFunc = do
  let svic = dataFromColumns []
        . dataColumn "Strike" (Numbers [k | (LogRel k) <- strikes])
        -- . dataColumn "Density" (Numbers [p | p <- map (impliedDensity b76)  strikes])
        . dataColumn "RSVI"   (Numbers [p | (TotalVar p) <- map (totalVar svi) strikes])
        . dataColumn "JWSVI" (Numbers [p | (TotalVar p) <- map (totalVar jw) strikes])

      encR = encoding
        . position X [PName "Strike", PmType Quantitative]
        . position Y [PName "RSVI", PmType Quantitative]
      encJW = encoding
        . position X [PName "Strike", PmType Quantitative]
        . position Y [PName "JWSVI", PmType Quantitative]
        -- . position Y [PName "TotalVar", PmType Quantitative]
      -- vl = toVegaLite [svic [], enc [], width 800, height 400, mark Line []]
      layers = layer [asSpec [encR [], mark Line [MColor "blue"]]
                     , asSpec [encJW  [], mark Line [MColor "red"]]
                     ]
      vl = toVegaLite [svic [], layers]
  toHtmlFile "/tmp/totalvar.html" vl


x :: RVar Double -> RVar(Double, Double)
x w = do
  w' <- w
  w'' <- w
  return (w', w'')

testQ = do
  let vol     = 0.2
      s       = 100
      drift   = constantDrift 0.02
      r       = ConstRate 0.02
      fwd     = YieldFwdCurve s r
      surface = ConstVolSurface vol
      dupire  = Dupire drift surface fwd
  testFwd dupire s (StdNormal::Normal Double)



main3 = testCEV
main2 = testQ
main1 = do
  u <- sviUniverse
  let spx   = fromJust (eqAsset (Ticker "SPX") u)
      vo    = spx ^. eqVolSurface
      drift = forwardCurveImpliedDrift spx
      dupire = Dupire drift vo spx
  print (spx ^. eqSpot)
  print $ (spx ^. eqFwd) False 5
  testProcess dupire (spx ^. eqSpot) (StdNormal::Normal Double) 0
  testProcess dupire (spx ^. eqSpot) (StdNormal::Normal Double) 0
  testProcess dupire (spx ^. eqSpot) (StdNormal::Normal Double) 0
  testProcess dupire (spx ^. eqSpot) (StdNormal::Normal Double) 0
--  testFwd dupire (spx ^. eqSpot) (StdNormal::Normal Double)
--  testFwd dupire (spx ^. eqSpot) (StdNormal::Normal Double)


plotUniverse1 u = do
  let spx   = fromJust (eqAsset (Ticker "SPX") u)
      vo    = spx ^. eqVolSurface
      drift = forwardCurveImpliedDrift spx
--      dupire = Dupire drift vo spx
      ts = [YearFrac t | t <- [0.1,0.5..5]]
      xs = [LogRel x | x <- [-1.6, -1.595..1.3]]
      f :: YearFrac -> [(Double, Double)]
      f t = map (\x-> (coerce x, coerce (surfaceTotalVarKT vo x t))) xs
      smiles = map f ts
  plotLists [] smiles


plotU1U2 :: (TimeSlice v1 LogRelStrike, TimeSlice v2 LogRelStrike) => Universe v2 -> Universe v1 -> YearFrac -> IO ()
plotU1U2 u1 u2 t = do
  let spx1   = fromJust (eqAsset (Ticker "SPX") u1)
      vo1    = spx1 ^. eqVolSurface
      spx2   = fromJust (eqAsset (Ticker "SPX") u2)
      vo2    = spx2 ^. eqVolSurface
      xs = [LogRel x | x <- [-1.6, -1.595..1.3]]
      smile1 = map (\x-> (coerce x::Double, coerce (surfaceTotalVarKT vo1 x t) :: Double)) xs
      smile2 = map (\x-> (coerce x, coerce (surfaceTotalVarKT vo2 x t))) xs
  plotLists [] [smile1, smile2]

sviUniverse :: IO (Universe SVI)
sviUniverse = puOrError "/Users/ghaisissa/Downloads/U20210917.json"

interpolatingUniverse :: IO (Universe (InterpolatingSmile LogRelStrike ))
interpolatingUniverse = puOrError "/Users/ghaisissa/Downloads/U20210917.json"

main4 = do
  u <- interpolatingUniverse
  u2 <- sviUniverse
  mapM_ (plotU1U2 u u2) [YearFrac t | t <- [1,2..6]]


foo :: State PureMT [Double]
foo = replicateM (length ts * 128000) (sample StdNormal)

oneTrajectory'' :: (StochasticProcess p s b, Monad m) => p -> s -> [YearFrac] -> [b] -> m Spot
oneTrajectory'' p s0 ts bs = let trajectory = do
                                              mapM_ (\(t, b) -> pEvolve' p b t) tsbs
                                              pSpot p <$> stateVariables
                                 dts        = zipWith (-) (tail ts) ts
                                 tsbs       = zip dts bs
                             in evalStateT (runSP trajectory) (0, s0)

trajectories'' :: (StochasticProcess p s b, Monad m) => p -> s -> [YearFrac] -> [[b]] -> m [Spot]
trajectories'' p s ts = mapM (oneTrajectory'' p s ts)

evolveWithRandoms = do
  let n = 128000
  mwc <- create
  bs1 <-  replicateM n $ replicateM (length ts) $ do
    x1 <- standard mwc
    x2 <- standard mwc
    return (x1, x2)
--  let bs = evalState foo (pureMT 0)
  let slv = exampleSLV 0.03 100
  finalSpots <- trajectories'' slv (Spot 100, Var 0) ts bs1
  print $ sum finalSpots / fromIntegral n
  print $ stdDev (V.fromList ((coerce finalSpots)::[Double]))



main = evolveWithRandoms


main5 = do
  let slv = exampleSLV 0.03 100
      slv' = slv--{_beta=1}
--  testProcess slv' (Spot 100, Var 0) t 0
--  testProcess slv' (Spot 100, Var 0) t 1
--  testProcess slv' (Spot 100, Var 0) t 2
--  testProcess slv' (Spot 100, Var 0) t 3
  testFwd slv' (Spot 100, Var 0) t
  where
      t :: UncorrelatedPair (Normal Double) (Double, Double)
      t = UncorrelatedPair StdNormal
