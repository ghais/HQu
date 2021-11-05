{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}
module Q.Stochastic.Discretize
        where

-- |Euler discretization of stochastic processes
newtype Euler = Euler { eDt :: Double }
        deriving stock (Show, Eq)

-- | Euler end-point discretization of stochastic processes
newtype EndEuler = EndEuler { eeDt :: Double }
        deriving stock (Show, Eq)


