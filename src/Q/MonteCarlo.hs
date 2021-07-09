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
import           Q.Stochastic.Discretize
import           Q.Stochastic.Process
import           Control.Monad
import           Q.ContingentClaim
import Data.Random
import Q.Time
import Data.Time
import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (standard)
import Q.ContingentClaim.Options
import Q.Types

type Path b = [(Time, b)]

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


type MonteCarlo s a = StateT [(Time, s)] RVar a


-- | Generate a single trajectory stopping at each provided time.
trajectory :: forall a b d. (StochasticProcess a b, Discretize d b) =>
             d        -- ^ Discretization scheme
           -> a        -- ^ The stochastic process
           -> b        -- ^ \(S(0)\)
           -> [Time]   -- ^ Stopping points \(\{t_i\}_i^n \) where \(t_i > 0\)
           -> [RVar b] -- ^ \(dW\)s. One for each stopping point.
           -> RVar [b] -- ^ \(S(0) \cup \{S(t_i)\}_i^n \) 
trajectory disc p s0 times dws = reverse <$> evalStateT (onePath times dws) initState' where
  initState' :: [(Time, b)]
  initState' = [(0, s0)]

  onePath :: [Time] -> [RVar b] -> MonteCarlo b [b]
  onePath [] _ = do
    s <- get
    return $ map snd s
  onePath (t1:tn) (dw1:dws) = do
    s <- get
    let t0 = head s
    b <- lift $ pEvolve p disc t0 t1 dw1
    put $ (t1, b) : s
    onePath tn dws

-- | Generate multiple trajectories. See 'trajectory'
trajectories:: forall a b d. (StochasticProcess a b, Discretize d b) =>
             Int        -- ^Num of trajectories
           -> d          -- ^Discretization scheme
           -> a          -- ^The stochastic process
           -> b          -- ^\(S(0)\)
           -> [Time]     -- ^Stopping points \(\{t_i\}_i^n \) where \(t_i > 0\)
           -> [RVar b]   -- ^\(dW\)s. One for each stopping point.
           -> RVar [[b]] -- ^\(S(0) \cup \{S(t_i)\}_i^n \) 
trajectories n disc p initState times dws = replicateM n $ trajectory disc p initState times dws

observationTimes :: ContingentClaim a -> [Day]
observationTimes = undefined

class Model a b | a -> b where
  discountFactor :: a -> YearFrac -> YearFrac -> RVar Rate
  evolve   :: a -> YearFrac -> StateT (YearFrac, b) RVar Double
