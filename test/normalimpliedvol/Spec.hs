module Main where
import           Control.Monad           (guard, unless, when)
import           Data.List               (intercalate)
import           Q.Options.Bachelier
import           Q.Options.ImpliedVol.Normal
import           Q.Options
import           Q.Types
import           Test.Hspec              hiding (shouldBe)
import           Test.Hspec.Expectations

closeTo x y =  compareWith (\x y -> (abs $ (x - y)) / (max x y) <= 1e-2) errorMessage x y where
  errorMessage = "Is not close to"
  compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)

test cp t (k, b@(Bachelier f r sigma))= do
  let v = euOption b t cp k
      p      = vPremium v
      sigma' = euImpliedVolWith Jackel cp f k t r p
      df     = discountFactor t r
  when (hasTimeValue cp f k p df) $
    it (intercalate ", " [show t, show f, show df, show cp, show k, show p, show sigma]) $ do
        sigma' `closeTo` sigma


runTests f r strikes vols t = do
  let bs        = [Bachelier f r sigma | sigma <- vols]
      testCases = [(k, b)              | k <-  strikes, b <- bs]
  context "Call Option" $ do
    mapM_ (test Call t) testCases
  context "Put Option" $ do
    mapM_ (test Put t) testCases

main = hspec $ do
  describe "bachelier european implied vol" $ do
    context "When asset price is positive ($100)" $ do
      let strikes   = [Strike k            | k <- [80,81..120]]
          vols      = [Vol sigma           | sigma <- [1,2..200]]
      let f = Forward 100
      context "1Y option" $ do
        let t = YearFrac 1
        context "When interest rate is zero (0%)" $ do
          let r         = Rate 0
          runTests f r strikes vols t
        context "When interest rate is slightly positive (1%)" $ do
          let r         = Rate 0.01
          runTests f r strikes vols t
        context "When interest rate is slightly negative (-1%)" $ do
          let r         = Rate (-0.01)
          runTests f r strikes vols t
