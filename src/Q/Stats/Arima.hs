{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Q.Stats.Arima where
import           Control.Monad.State
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Random
import           Data.Random.Source
import           Data.RVar
import           Data.Time
import           Numeric.LinearAlgebra
import           Q.Stats.TimeSeries
import           System.Random.Mersenne.Pure64
import           Data.Random.Distribution
import           Data.Random.Distribution.Poisson
import           Data.Random.Distribution.T
import           Data.RVar
import           Statistics.Sample

data Ewma d = Ewma Double d

--ll :: (Ewma d) -> [DataPoint Double] -> (Double -> Double)
ll (Ewma lambda d) datapoints = mapM ll_ datapoints where
  ll_ :: DataPoint LocalTime Double -> State Double Double
  ll_ x@(DataPoint _ v) = do
    vart <- get
    let vart2 = lambda * vart + (1 - lambda) * v * v
    put vart2
    return $ logPdf d (sqrt (v  * v / vart))


--forecast :: (Distribution d Double) => (Ewma d) -> Int ->
forecast :: forall d. (Distribution d Double) => Ewma (d Double) -> StateT Double RVar Double
forecast (Ewma lambda d) = do
  y <- lift $ rvar d
  vart <- get
  let vart2 = lambda * vart + (1 - lambda) * y * y
  put vart2
  return (y * sqrt vart)


--forecastN :: Distribution d Double => Ewma (d Double) -> Int -> Double -> RVar ([Double], Double)
forecastN ewma var0 n =  sample $ runStateT (replicateM n (forecast ewma)) var0

