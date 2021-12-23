{- |
Copyright: (c) 2021 Ghais
SPDX-License-Identifier: MIT
Maintainer: Ghais <0x47@0x49.dev>

General purpose quantitative finance library
-}

module HQu where

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
      LogRelStrike(LogRel),
      Spot(Spot),
      Strike(Strike),
      Theta(Theta),
      TotalVar(TotalVar),
      Var(Var),
      Vol(Vol),
      YearFrac(YearFrac) )
import Data.RVar (RVar)
import Q.Options.ImpliedVol.Surface (surfaceTotalVarKT, ConstVolSurface (ConstVolSurface))
import Data.Coerce
import Q.Stochastic.Process hiding (rho)




import Data.Random.Distribution
import Data.Random.Distribution.Normal (Normal(StdNormal), stdNormal)
import Q.Stochastic.DupireLocalVol
import Q.TermStructures (FwdCurve(..))
import Q.TermStructures.Yield.DiscountCurve (ConstRate(ConstRate))
import Control.Monad (replicateM)



alpha = Alpha (1)
beta = Beta 0
rho = Rho (0)
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
      f x = (max (100 * (exp x) - 100) 0) * (impliedDensity svi (LogRel x))
  print truePrice
  print integrationPrice


heston = HestonModel (constantDrift 0.01) (Kappa 3) (Theta 0.02) (Sigma 0.4) (Rho (-0.6))
gbm = GBM 0.01 0.1
ts  = map YearFrac [0,0.005..1]


testProcess :: (Distribution d b, StochasticProcess p s b) => p -> s -> d b -> IO ()
testProcess p s d= do
  gbms <- oneTrajectory p s d  ts
  plotList [] (zip ((coerce (tail ts))::[Double]) ((coerce gbms)::[Double]))

testFwd :: (Distribution d b, StochasticProcess p s b) => p -> s -> d b -> IO ()
testFwd p s d = do
  gbms <- replicateM 16000 (oneTrajectory p s d  ts)
  let finalSpots = map (\trajectory -> last trajectory) gbms
  print $ (sum finalSpots) / 16000
testGBM :: IO ()
testGBM = testProcess gbm (Spot 100) (StdNormal::Normal Double)

testHeston :: IO ()
testHeston = testProcess heston (Spot 100, Var 0.4) t where
  t :: UncorrelatedPair (Normal Double) (Double, Double)
  t = UncorrelatedPair StdNormal



myssvi = let vol = Vol 0.2
             rho  = Rho (0.3)
             lambda = Lambda 0.9
         in SSVI (`volToTotalVar` vol) rho lambda


testDupire = do
  let drift = constantDrift 0.05
      s     = Spot 100
      ssvi  = myssvi
      fwd   = YieldFwdCurve s (ConstRate 0.05)
      dupire = Dupire drift ssvi fwd
    in testProcess dupire (Spot 100) (StdNormal::Normal Double)
testSSVI = do
  let vol = Vol 0.2
      rho  = Rho (0.3)
      lambda = Lambda 0.9
      ssvi = SSVI (`volToTotalVar` vol) rho lambda
      ts   = [0.05, 0.1, 0.2, 0.3, 0.5]::[Double]
      ks   = [-0.9, -0.89..1.2]::[Double]
  plotFunc3d [] [] (ks) ts (\k t -> coerce $ surfaceTotalVarKT ssvi (LogRel k) (YearFrac t)::Double)


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





