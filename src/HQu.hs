{- |
Copyright: (c) 2021 Ghais
SPDX-License-Identifier: MIT
Maintainer: Ghais <0x47@0x49.dev>

General purpose quantitative finance library
-}

module HQu where

import Q.Options.ImpliedVol.TimeSlice
import Q.Options.ImpliedVol.SVI
import           Graphics.Vega.VegaLite      hiding (repeat, sample)
import Q.Options.Black76 
import Numeric.GSL.Integration

import Q.Types hiding (Rho)
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
