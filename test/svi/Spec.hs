{-# LANGUAGE FlexibleContexts #-}
module Main where

import Test.Hspec hiding (shouldBe)
import Q.Options.ImpliedVol.SVI
import Q.Types hiding (Rho)
import Q.Options.ImpliedVol.TimeSlice (totalVar)

import Control.Monad (unless)

import Graphics.Gnuplot.Simple
import Data.Coerce (coerce)

closeTo :: TotalVar -> TotalVar -> Expectation
closeTo = compareWith (\x y -> abs (x - y) <= 1e-7) errorMessage where
  errorMessage = "Is not close to"
  compareWith :: ( Show a) =>(a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)




skewTest =
  let
    ttm   = YearFrac 1
    alpha = Alpha 0.1
    beta  = Beta 0.2
    rho   = Rho 0
    m     = M 0
    sigma = Sigma 0.1
    svi = RSVI ttm alpha beta rho m sigma
    strikes = [LogRel k | k <- [-1, -0.9..4]]
    totalVars = map (totalVar svi)  strikes
  in do
    print strikes
    print totalVars
    plotList [] ((coerce (zip strikes totalVars))::([(Double, Double)]))

main :: IO ()
main = hspec $
  describe "SVI" $ do
  context "Converting R-SVI to JW-SVI" $ do
    let alpha = Alpha (0.2)
        beta = Beta 0.11
        rho = Rho (0.2)
        m = M 0
        sigma = Sigma 0.2
        ttm = YearFrac 0.01
        rsvi = RSVI ttm alpha beta rho m sigma
        jwsvi   = rSVIToJW ttm alpha beta rho m sigma

        strikes = [LogRel k | k <- [-2,-1.9..3]]
    let check k = let rv = totalVar rsvi k
                      jv = totalVar jwsvi k
                  in it ("JW-SVI and RSVI have the same total var at " ++ show k) $ do
                      rv `closeTo` jv
    mapM_ check strikes
  context "Converting JW-SVI to R-SVI" $ do
    let ttm = YearFrac 0.01
        vt    = V 22.2
        phit  = Phi 0.02
        pt    = P 0.19
        ct    = C 0.3
        vtilt = VTil 22.2
        jwsvi = JWSVI ttm vt phit pt ct vtilt
        rsvi'   = jwSVIToR ttm vt phit pt ct vtilt
        strikes = [LogRel k | k <- [-2,-1.9..3]]
    let check k = let rv = totalVar rsvi' k
                      jv = totalVar jwsvi k
                  in it ("JW-SVI and RSVI have the same total var at " ++ show k) $ do
                      rv `closeTo` jv
    mapM_ check strikes



