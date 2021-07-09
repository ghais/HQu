{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}
module Q.Stochastic.Discretize
        where

import           Data.Functor
import           Data.RVar
import           Numeric.LinearAlgebra
import           Q.Stochastic.Process
-- |Euler discretization of stochastic processes
newtype Euler = Euler { eDt :: Double }
        deriving (Show, Eq)

-- | Euler end-point discretization of stochastic processes
newtype EndEuler = EndEuler { eeDt :: Double }
        deriving (Show, Eq)


instance Discretize Euler Double where
  dDrift p Euler{..} s0 = pDrift p s0 <&> (* eDt)
  dDiff  p Euler{..} b  = (pDiff p b) <&> (* (sqrt eDt))
  dDt    _ Euler{..} _  = eDt

instance Discretize Euler (Vector Double) where
  dDrift p Euler{..} s0 = pDrift p s0 <&> (scale eDt)
  dDiff  p Euler{..} b = (pDiff p b) <&> (scale (sqrt eDt))
  dDt    _ Euler{..} _  = eDt

instance (forall a b. StochasticProcess a Double) => Discretize EndEuler Double where
  dDrift p EndEuler{..} s0@(t0, x0) = pDrift p (t0 + eeDt, x0) <&> (* eeDt)
  dDiff  p EndEuler{..}  s0@(t0, x0) =  pDiff  p (t0 + eeDt, x0) <&> (* (sqrt eeDt))
  dDt    _ e _   = eeDt e
