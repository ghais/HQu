{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Q.MonteCarlo where
import           Control.Monad.State
import           Data.RVar










import Q.Types



type Path b = [(YearFrac, b)]

-- |Summary type class aggregates all priced values of paths
class (PathPricer p)  => Summary m p | m->p where
  -- | Updates summary with given priced pathes
  sSummarize      :: m -> [p] -> m

  -- | Defines a metric, i.e. calculate distance between 2 summaries
  sNorm           :: m -> m -> Double

-- | Path generator is a stochastic path generator
class PathGenerator m where
  pgMkNew         :: m->IO m
  pgGenerate      :: Integer -> m -> Path b

-- | Path pricer provides a price for given path
class PathPricer m where
  ppPrice :: m -> Path b -> m


type MonteCarlo s a = StateT [(YearFrac, s)] RVar a



class Model a b | a -> b where
  discountFactor :: a -> YearFrac -> YearFrac -> RVar Rate
  evolve   :: a -> YearFrac -> StateT (YearFrac, b) RVar Double
