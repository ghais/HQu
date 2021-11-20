module Main where
import Test.Hspec hiding (shouldBe)
import Q.Options.Bachelier
import Q.Types
import Q.Options
import           Control.Monad (unless)


closeTo x y =  compareWith (\x y -> (abs $ (x - y)) <= 1e-5) errorMessage x y where
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
      theta = vTheta expected
      rho   = vRho expected
  it ("is priced at " ++ (show p)) $ do
    vPremium v `closeTo` p
  it ("has a " ++ (show delta)) $ do
    vDelta v `closeTo` delta
  it ("has a " ++ (show vega)) $ do
    vVega v `closeTo` vega
  it ("has a " ++ (show gamma)) $ do
    vGamma v `closeTo` gamma
  it ("has a " ++ (show theta)) $ do
    vTheta v `closeTo` theta
  it ("has a " ++ (show rho)) $ do
    vRho v `closeTo` rho

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
                b = Bachelier t f r vol
                v = eucall b k
            let expected = Valuation
                           (Premium 7.9788456)
                           (Delta 0.5)
                           (Vega 0.3989422)
                           (Gamma 0.01994711)
                           (Theta (-0.010937))
                           (Rho 0.500008)
            testOptionValuation b k t v expected
          context "1Y 'Put' option atm strike ($100)" $ do
            let k = Strike 100
                t = YearFrac 1
                b = Bachelier t f r vol
                v = euput b k
            let expected = Valuation
                           (Premium 7.9788456)
                           (Delta 0.5)
                           (Vega 0.3989422)
                           (Gamma 0.01994711)
                           (Theta (-0.010937))
                           (Rho 0.5)
            testOptionValuation b k t v expected

