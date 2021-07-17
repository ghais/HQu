module Main where

import           Test.Hspec

import           Q.Options.Black76
import           Q.Options.Pricing.Curran
import qualified Q.SortedVector as SV

import           Control.Monad.Except
import           Data.Coerce (coerce)
import           Data.Either
import           Q.Options (intrinsic, Valuation (vPremium))
import           Q.Types
closeTo :: Double -> Double -> Expectation
closeTo = compareWith (\x y -> abs (x - y) <= 1e-7) errorMessage where
  errorMessage = "Is not close to"
  compareWith :: ( Show a) =>(a -> a -> Bool) -> String -> a -> a -> Expectation
  compareWith comparator errorDesc result expected  = expectTrue errorMsg (comparator expected result)
    where errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected
  expectTrue msg b = unless b (expectationFailure msg)


main :: IO ()
main = hspec $
  describe "Q.Options.Pricing.Curran.asianOption" $ do
    it "throws an error if no monitoring times are provided" $ do
      let b76 = Black76 (Forward 100) (DF 1) (YearFrac 1) (Vol 1)
          monitoringTimes = SV.fromList []
          nFixed = 10
          sFixed = 100
          cp = Call
          k  = Strike 10
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldSatisfy` isLeft
    it "throws an error if time to first monitoring point is after expiry" $ do
      let b76 = Black76 (Forward 100) (DF 1) (YearFrac 1) (Vol 1)
          monitoringTimes = SV.fromList [2]
          nFixed = 10
          sFixed = 100
          cp = Call
          k  = Strike 10
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldSatisfy` isLeft
    it "throws an error if num fixed points is negative" $ do
      let b76 = Black76 (Forward 100) (DF 1) (YearFrac 1) (Vol 1)
          monitoringTimes = SV.fromList [0.5]
          nFixed = -1
          sFixed = 0
          cp = Call
          k  = Strike 10
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldSatisfy` isLeft
    it "throws an error if sum so far is positive but nFixed is 0" $ do
      let b76 = Black76 (Forward 100) (DF 1) (YearFrac 1) (Vol 1)
          monitoringTimes = SV.fromList [0.5]
          nFixed = 0
          sFixed = 100
          cp = Call
          k  = Strike 10
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldSatisfy` isLeft
    it "throws an error if we are on expiry but not all points are fixed" $ do
      let b76 = Black76 (Forward 100) (DF 1) (YearFrac 0) (Vol 1)
          monitoringTimes = SV.fromList [0.5, 0.7]
          nFixed = 1
          sFixed = 100
          cp = Call
          k  = Strike 10
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldSatisfy` isLeft
    it "should return 0 if we are past expiry" $ do
      let b76 = Black76 (Forward 100) (DF 1) (YearFrac (-0000.1)) (Vol 1)
          monitoringTimes = SV.fromList [-0.7, -0.5]
          nFixed = 2
          sFixed = 100
          cp = Call
          k  = Strike 10
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldBe` Right (Premium 0)
    it "should return intrinsic if we are on expiry" $ do
      let b76 = Black76 (Forward 100) (DF 1) (YearFrac 0) (Vol 1)
          monitoringTimes = SV.fromList [-0.7, -0.5]
          nFixed = 2
          sFixed = 100
          cp = Call
          k  = Strike 10
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldBe` Right (Premium 40)
    it "should return discounted intrinsic if volatility is 0" $ do
      let f = Forward 100
          df = DF 0.5
          b76 = Black76 f df (YearFrac 1) (Vol 0.00000000000001)
          monitoringTimes = SV.fromList [-0.5, 1]
          nMonitoringTimes = fromIntegral (SV.length monitoringTimes) :: Double
          nRemainingFixings  = fromIntegral (SV.length monitoringTimes) - fromIntegral nFixed :: Double
          nFixed = 1
          sFixed = 50
          cp = Call
          k  = Strike 70
          finalAvg = Forward ((coerce f * nRemainingFixings + sFixed) / nMonitoringTimes)
          pv = discount df $ intrinsic  cp finalAvg k
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldBe` Right (Premium pv)
    it "should return discounted intrinsic if the option is guaranteed to be in the money" $ do
      let f = Forward 100
          df = DF 0.5
          b76 = Black76 f df (YearFrac 1) (Vol 1)
          monitoringTimes = SV.fromList [-0.5, 0.5, 0.7, 1]
          nMonitoringTimes = fromIntegral (SV.length monitoringTimes) :: Double
          nFixed = 1
          nRemainingFixings  = fromIntegral (SV.length monitoringTimes) - fromIntegral nFixed :: Double
          sFixed = 50
          cp = Call
          k  = Strike 10
          finalAvg = (sFixed + nRemainingFixings * coerce f) / nMonitoringTimes
          pv = discount df $ intrinsic  cp (Forward finalAvg) k
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldBe` Right (Premium pv)
    it "should be priced like a European option if there is exactly one fixing" $ do
      let f = Forward 100
          df = DF 0.96
          b76 = Black76 f df (YearFrac 1) (Vol 1)
          monitoringTimes = SV.fromList [1]
          nFixed = 0
          sFixed = 0
          cp = Call
          k = Strike 100
          pv = euOption b76 cp k
      asianOption b76 monitoringTimes nFixed sFixed cp k `shouldBe` Right (vPremium pv)
      




