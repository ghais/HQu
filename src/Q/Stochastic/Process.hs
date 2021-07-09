{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Q.Stochastic.Process
        where
import           Control.Monad
import           Control.Monad.State
import           Data.List             (foldl')
import           Data.RVar
import           Data.Random
import           Numeric.LinearAlgebra

rwalkState :: RVarT (State Double) Double
rwalkState = do
    prev <- lift get
    change  <- rvarT StdNormal

    let new = prev + change
    lift (put new)
    return new

type Time = Double

-- Dont know why this wasn't done.
-- Is there an easier way to do this where we either lift or return?
instance (Num a) => Num (RVarT m a) where
  (+) = liftM2 (+)
  (-) = liftM2 (-)
  (*) = liftM2 (*)
  abs = liftM abs
  signum = liftM signum
  fromInteger x = return $ fromInteger x



-- |Discretization of stochastic process over given interval
class (Num b) => Discretize d b where
  -- |Discretization of the drift process.
  dDrift  :: (StochasticProcess a b) => a -> d -> (Time, b) -> RVar b
  -- |Discretization of the diffusion process.
  dDiff   :: (StochasticProcess a b) => a -> d -> (Time, b) -> RVar b
  -- |dt used.
  dDt     :: (StochasticProcess a b) => a -> d -> (Time, b) -> Time


-- |A stochastic process of the form \(dX_t = \mu(X_t, t)dt + \sigma(S_t, t)dB_t \)
class (Num b) => StochasticProcess a b where
  -- |The process drift.
  pDrift  :: a -> (Time, b) -> RVar b
  -- |The process diffusion.
  pDiff   :: a -> (Time, b) -> RVar b

  -- |Evolve a process from a given state to a given time.
  pEvolve :: (Discretize d b) => a         -- ^The process
                             -> d         -- ^Discretization scheme
                             -> (Time, b) -- ^Initial state
                             -> Time      -- ^Target time t.
                             -> RVar b    -- ^\(dB_i\).
                             -> RVar b    -- ^\(X(t)\).
  pEvolve p disc s0@(t0, x0) t dw = do
    if t0 >= t then return x0 else do
      s'@(t', b') <- pEvolve' p disc s0 dw
      if t' >= t then return b' else pEvolve p disc s' t dw

  -- |Similar to evolve, but evolves the process with the discretization scheme \(dt\).
  pEvolve' :: (Discretize d b, Num b) => a -> d -> (Time, b) -> RVar b -> RVar (Time, b)
  pEvolve' process discr s@(t, b) dw = do
    let !newT = t + dDt process discr s
        !newX = do
               drift <- dDrift process discr s
               diff  <- dDiff process discr s
               dw' <- dw
               return $ b + drift + diff * dw'
        newX :: RVar b

    (newT,) <$>  newX

-- |Geometric Brownian motion
data GeometricBrownian = GeometricBrownian {
    gbDrift :: Double -- ^Drift
  , gbDiff  :: Double -- ^Vol
} deriving (Show)


instance StochasticProcess GeometricBrownian Double where
--  pDrift :: GeometricBrownian -> (Time, Double) -> RVar Double
  pDrift p (_, x) = return $ gbDrift p * x -- drift is prpotional to the spot.
  pDiff  p (_, x) = return $ gbDiff p  * x -- diffisuion is also prportional to the spot.


-- | Ito process
data ItoProcess = ItoProcess {
        ipDrift :: (Time, Double) -> Double,
        ipDiff  :: (Time, Double) -> Double
}
