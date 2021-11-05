module Main where

import           Graphics.Gnuplot.Simple

import qualified Q.TermStructures.Yield.DiscountCurve as DiscoutCurve

import           Q.Time.DayCounter (DayCounter (Thirty360USA), dcYearFraction)

import           Data.Coerce
import qualified Data.SortedList as SortedList
import           Data.Time
import           Q.Options.ImpliedVol.SVI
import           Q.Options.ImpliedVol.TimeSlice
import           Q.TermStructures (YieldTermStructure (yieldDiscount))
import           Q.Time.Date
import           Q.Types (DF (..), LogRelStrike (..), TotalVar (..), YearFrac (..), totalVarToVol, Vol(..), Forward(..))

import Numeric.LevMar
import qualified Data.Vector.Storable as V


d :: Day
d = fromGregorian 2015 3 31
dc :: DayCounter
dc = Thirty360USA
cal :: Calendar
cal = NullGregorian

dfs :: SortedList.SortedList (Day, DF)
dfs = SortedList.toSortedList [(fromGregorian 2016 1 1, DF 0.4)]

curve = case DiscoutCurve.mkDiscountCurve d dc cal 0 dfs of
  Left err -> error err
  Right v  -> v

dates :: [Day]
dates = take 600 (map (\i -> addDays i d) [0..])
values :: [Double]
values = map (coerce (yieldDiscount curve)) dates
ts :: [Double]
ts = (map (dcYearFraction dc d) dates)



-- Define 3 parameter SUV
ttm   = YearFrac 1
f     = Forward 100
alpha = (Alpha 0.1)
rho   = (Rho (0.2))
sigma = (Sigma 0.5)
svi   = SVI3P ttm alpha  rho sigma

-- define 3 strikes
k1 = (LogRel (99/100))
k2 = (LogRel (98/100))
k3 = (LogRel (97/100))

-- Calculate total var at k1, k2, k3
var1 = totalVar svi k1
var2 = totalVar svi k2
var3 = totalVar svi k3

-- Test that analytical solution works as long as we have ATM vol.
vol1 = totalVarToVol var1 ttm
vol2 = totalVarToVol var2 ttm
atmVar = totalVar svi (LogRel 0)
atm  = totalVarToVol atmVar ttm
-- Solve analytically from atm k1 and k2
svi' = analyticalSVI3P ttm (atm) (k1, vol1) (k2, vol2)

strikes    = [LogRel (log (k/ (coerce f))) | k <- [1..500]]
totalVars  = map (totalVar svi) strikes
totalVars' = map (totalVar svi') strikes




model :: Model Double
model p = let alpha = Alpha $ p V.! 0
              rho   = Rho   $ p V.! 1
              sigma = Sigma $ p V.! 2
              svi'  = SVI3P ttm alpha rho sigma
          in V.fromList $ coerce $ (map (totalVar svi')) [k1, k2, k3]


targets = map (totalVar svi) [k1, k2, k3]


svi'' = case (levmar model Nothing (V.fromList [0.3,0,0.5]) (V.fromList (coerce targets)) 1000 defaultOpts mempty) of
  Left err -> error (show err)
  Right (r, _, _) -> let alpha = Alpha $ r V.! 0
                         rho   = Rho   $ r V.! 1
                         sigma = Sigma $ r V.! 2
                      in SVI3P ttm alpha rho sigma
totalVars'' = map (totalVar svi'') strikes
main = do
  print svi
  print svi'
  print svi''
  print $ totalVar svi k1
  print $ totalVar svi'' k1
  print $ totalVar svi k2
  print $ totalVar svi'' k2
  print $ totalVar svi k3
  print $ totalVar svi'' k3
  plotList [Title "Original SVI"] ((coerce (zip strikes totalVars))::([(Double, Double)]))
  plotList [Title "Analytical SVI"] ((coerce (zip strikes totalVars'))::([(Double, Double)]))
  plotList [Title "Calibrated SVI from 3 points"] ((coerce (zip strikes totalVars''))::([(Double, Double)]))
  


