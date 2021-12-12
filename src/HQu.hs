{- |
Copyright: (c) 2021 Ghais
SPDX-License-Identifier: MIT
Maintainer: Ghais <0x47@0x49.dev>

General purpose quantitative finance library
-}

module HQu where

import Q.Options.ImpliedVol.TimeSlice
import Q.Options.ImpliedVol.SVI
import           Graphics.Vega.VegaLite      hiding (Theta, repeat, sample)
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
import Q.Options.ImpliedVol.Surface (surfaceTotalVarKT)
import Data.Coerce
import Q.Stochastic.Process hiding (rho)




import Data.Random.Distribution
import Data.Random.Distribution.Normal (Normal(StdNormal), stdNormal)
import Q.Universe.IR
import Q.TermStructures

alpha = Alpha (1)
beta = Beta 0
rho = Rho (0)
m = M 0
sigma = Sigma 0
ttm = YearFrac 1

jw = JWSVI ttm (V 1) (Phi 0.0) (P 0.0000) (C 0.00000) (VTil 1)
svi = RSVI ttm alpha beta rho m sigma

f = Forward 100
strikes = [LogRel (log (k $/$ f)) | k <- [1..2000]]
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
ts  = map YearFrac [0,0.002..4]


testProcess :: (Distribution d b, StochasticProcess p s b) => p -> s -> d b -> IO ()
testProcess p s d= do
  gbms <- oneTrajectory p s d  ts
  plotList [] (zip ((coerce (tail ts))::[Double]) ((coerce gbms)::[Double]))



testGBM :: IO ()
testGBM = testProcess gbm (Spot 100) (StdNormal::Normal Double)

testHeston :: IO ()
testHeston = testProcess heston (Spot 100, Var 0.4) t where
  t :: UncorrelatedPair (Normal Double) (Double, Double)
  t = (UncorrelatedPair StdNormal)

testSSVI = do
  let vol = Vol 0.2
      rho  = Rho (0.3)
      lambda = Lambda 0.9
      ssvi = SSVI (volToTotalVar vol) rho lambda
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





testIR = do
  ir <- irFromFile "/tmp/a.json"
  case ir of
    Left str -> error str
    Right p -> printDiscount p


printDiscount IR{..} = do
  let ts  = map YearFrac [0, 0.01..10]
      dfs = map (yieldDiscountT _ibor1mCurve) ts
  plotList [] (zip (coerce ts::[Double]) (coerce dfs::[Double]))
