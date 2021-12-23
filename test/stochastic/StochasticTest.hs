module Main where

import Q.Stochastic.HypHyp
import Q.Types
import Control.Monad (unless)
import Q.Stochastic.Process
import           Test.Hspec hiding (shouldBe)




closeTo x y =  compareWith (\x y -> (abs $ (x - y)) <= 1e-5) errorMessage x y where
  errorMessage = "Is not close to"
  compareWith :: ( Show a) =>(a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)




testLeverage :: IO ()
testLeverage = hspec $ do
  describe "HypHyp leverage test" $ do
    let u  = constantDrift 0
        s0 = 100
    context "beta is 1 leverage is equal to vol" $ do
      let s      = Spot 100
          beta   = 1
          alpha  = undefined
          kappa  = undefined
          rho    = undefined
          sigmas  = [0,0.1..1]
          hyphyps = map (HypHyp u s0 beta alpha kappa rho) sigmas
      mapM_ (\h@HypHyp{..} -> it ("Has " ++ (show _sigma)) ((slvLeverage h s undefined undefined) `closeTo` _sigma)) hyphyps
    context "When s(0) = s(t) = 100 we get back sigma" $ do
      let s      = 100
          betas  = [0.1, 0.2..2]
          alpha  = undefined
          kappa  = undefined
          rho    = undefined
          sigma  = 0.1
          hyphyps = map (\b -> HypHyp u s0 b alpha kappa rho sigma) betas
      mapM_ (\h@HypHyp{..} -> it ("Has " ++ (show _beta)) ((slvLeverage h s undefined undefined) `closeTo` _sigma)) hyphyps

testGamma = hspec $ do
  describe "HypHyp gamma test" $ do
    
main = testLeverage
