{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec hiding (shouldBe)
import Q.Options.Bachelier
import Q.Types
import Test.Hspec.Expectations
import           Control.Monad (unless)
import Q.SortedVector
closeTo x y =  compareWith (\x y -> (abs $ (x - y)) <= 1e-7) errorMessage x y where
  errorMessage = "Is not close to"
  compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)

testOptionValuation b k t v expected = do
  let p     = vPremium expected
      delta = vDelta expected
      vega  = vVega expected
      gamma = vGamma expected
  it ("is priced at " ++ (show p)) $ do
    vPremium v `closeTo` p
  it ("has a " ++ (show delta)) $ do
    vDelta v `closeTo` delta
  it ("has a " ++ (show vega)) $ do
    vVega v `closeTo` vega
  it ("has a " ++ (show gamma)) $ do
    vGamma v `closeTo` gamma


main :: IO ()
main = hspec $ do
  describe "bachelier" $ do
    context "When asset price is positive ($100)" $ do
      let f = Forward 100
      context "When interest rate is zero (0%)" $ do
        let r = Rate 0
        context "When volatility is $20" $ do
          let vol = Vol 20
          context "1Y 'Call' option atm strike ($100)" $ do
            let k = Strike 100
                t = YearFrac 1
                b = Bachelier f r vol
                v = eucall b t k
            let expected = Valuation
                           (Premium 7.9788456)
                           (Delta 0.5)
                           (Vega 0.3989422)
                           (Gamma 0.01994711)
            testOptionValuation b k t v expected
          context "1Y 'Put' option atm strike ($100)" $ do
            let k = Strike 100
                t = YearFrac 1
                b = Bachelier f r vol
                v = euput b t k
            let expected = Valuation
                           (Premium 7.9788456)
                           (Delta 0.5)
                           (Vega 0.3989422)
                           (Gamma 0.01994711)
            testOptionValuation b k t v expected

