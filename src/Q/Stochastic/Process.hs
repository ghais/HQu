
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}


{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE BangPatterns #-}


module Q.Stochastic.Process
        where
import           Control.Monad.State.Strict
import           Data.RVar
import           Data.Random
import GHC.Exts

import Q.Types
    ( Var(..),
      Spot(..),
      DF(..),
      YearFrac(..),
      Forward(..),
      Rate(..),
      Theta(..),
      Rho(..),
      Sigma(..),
      Kappa(..), ($/$) )

import Q.TermStructures (YieldTermStructure, yieldDiscountT, ForwardCurveTermStructure (tsForwardT), tsForwardT)



newtype ProcessDrift  = ProcessDrift {integratedDrift :: YearFrac -> YearFrac -> Double}


newtype ConstDistribution t = ConstDistribution t
instance Distribution ConstDistribution t where
  rvar (ConstDistribution t) = return t

newtype UncorrelatedPair d t = UncorrelatedPair d

instance (Distribution d b) => Distribution (UncorrelatedPair (d b)) (b, b) where
  rvar (UncorrelatedPair d) = do
    x1 <- rvar d
    x2 <- rvar d
    return (x1, x2)

drift :: ProcessDrift -> YearFrac -> YearFrac -> Double
drift (ProcessDrift f) = f

constantDrift :: Rate -> ProcessDrift
constantDrift (Rate mu) = ProcessDrift (\_ _ -> mu)

discountImpliedDrift :: (YieldTermStructure r) => r -> ProcessDrift
discountImpliedDrift r = ProcessDrift u
  where u t dt | dt <= 1e-6 = u t 1e-6
               | otherwise = log (yieldDiscountT r t / yieldDiscountT r (t + dt)) $/$ dt


forwardCurveImpliedDrift :: (ForwardCurveTermStructure f) => f -> ProcessDrift
forwardCurveImpliedDrift f = ProcessDrift u
  where u t dt | dt <= 1e-6 = u t 1e-6
               | otherwise = log (tsForwardT f (t + dt) / tsForwardT f t) $/$ dt

newtype StochasticProcessMonad s m a = PM {runSP :: StateT (YearFrac, s) m a} deriving newtype (Functor, Applicative, Monad, MonadTrans , MonadState (YearFrac, s))



stateVariables :: Monad m => StochasticProcessMonad s m s
stateVariables = gets snd


time :: Monad m => StochasticProcessMonad s m YearFrac
time  = gets fst

class HasSpot a where
  spot :: a -> Spot


class ProcessState p s where
  pSpot :: p -> s -> Spot


-- |A stochastic process of the form \(dX_t = \mu(X_t, t)dt + \sigma(S_t, t)dB_t \)
class (ProcessState p s) => StochasticProcess p s b where
  pEvolve :: (MonadRandom m, Distribution d b) => p -> d b -> YearFrac -> StochasticProcessMonad s m Spot
  pEvolve p d dt = do
    !b <- lift $ sample d
    pEvolve' p b dt

  pEvolve' :: (Monad m) => p -> b -> YearFrac -> StochasticProcessMonad s m Spot


class LocalVol lv where
  lvDrift     :: lv -> ProcessDrift
  lvLogSigmaSvt :: lv -> Spot -> YearFrac -> Double

  lvLogSigmaSvtdt :: lv -> Spot -> YearFrac -> YearFrac -> Double
  lvLogSigmaSvtdt lv s t dt = (lvLogSigmaSvt lv s t + lvLogSigmaSvt lv s (t + dt))/2

  lvLogDriftSvtDt :: lv -> Spot -> YearFrac -> YearFrac -> Double
  lvLogDriftSvtDt lv s t dt  = let vol =  lvLogSigmaSvtdt lv s t dt
                               in drift (lvDrift lv) t dt  - 0.5 * vol * vol;


instance  ProcessState lv Spot where
  pSpot _ s = s

instance {-# OVERLAPPABLE #-} (LocalVol lv) => StochasticProcess lv Spot Double where
  pEvolve' lv dW (YearFrac dt) = do
    s  <- stateVariables
    t  <- time
    let s' = s * coerce (exp (lvLogDriftSvtDt lv s t (YearFrac dt) * dt + lvLogSigmaSvtdt lv s t (YearFrac dt) * dW * sqrt dt))
    put (t + coerce dt, s')
    return s'

-- | A stochastic local vol process which satisfies
-- |
-- | \( dS(t)/S(t) = \mu(t) * dt + L(S(t), t) * \gamma(V(t)) * dW1(t) \)
-- |
-- | \( dV(t) = \mu_v(V(t)) * dt + \sigma_v(V(t)) * dW2(t) \)
-- |
-- | \( dW1(t) * dW2(t)  = \rho * dt \)
-- |
-- | The model in log space is:
-- | \(dln(S(t)) =  (\mu(t) - 0.5 * L^2(S, t) * \gamma^2(V(t))) * dt + L(S(t), t) * \gamma(V(t)) * dW1(t) \)
class SLVProcess p where
  -- | The term \( L(S(t), t) \) average over [t, t+dt]
  slvLeverage      :: p -> Spot -> YearFrac -> YearFrac -> Double
  -- | The vol component \( \gamma(V) \)
  slvGamma         :: p -> Var -> Double
  -- | The drift component \( \mu(t) \) averaged over [t, t+dt]
  slvDrift         :: p -> ProcessDrift

  -- | Correlated two brownian motions.
  slvCorrelate     :: p -> (Double, Double) -> (Double, Double)

  -- | Drift \( mu_v(V) \) the variance process.
  slvVarianceDrift :: p -> Var -> Double
  -- | Vol of variance \( \sigma_v(V) \)
  slvVarianceVol   :: p -> Var -> Double



slvLogDriftSvt :: SLVProcess p => p -> Spot -> Var -> YearFrac -> YearFrac -> Double
slvLogDriftSvt p s v t dt =
  let u   = drift (slvDrift p) t dt
      vol = slvLeverage p s t dt * slvGamma p v
  in u - vol * vol / 2

-- | A stochasic vol process where the model is:
-- |
-- | \( dS(t)/S(t) =  \mu(t) * dt + \gamma(V(t)) * dW1(t) \)
-- |
-- | \( dV(t) = \mu_v(V(t)) * dt + \sigma_v(V(t)) * dW2(t) \)
-- |
  -- | The model in log space is:
-- | \( dln(S(t)) =  (\mu(t) - 0.5 * \gamma^2(v)) * dt + \gamma(v) * dW1(t) \)
class StochasticVol p where
  svGamma :: p -> Var -> Double
  svDrift :: p -> ProcessDrift
  svCorrelate :: p -> (Double, Double) -> (Double, Double)
  svVarianceDrift :: p -> Var -> Double
  svVarianceVol   :: p -> Var -> Double



instance {-# OVERLAPPABLE #-} (StochasticVol p) => SLVProcess p where
  slvLeverage _ _ _ _ = 1
  slvGamma            = svGamma
  slvDrift            = svDrift
  slvCorrelate        = svCorrelate
  slvVarianceDrift    = svVarianceDrift
  slvVarianceVol      = svVarianceVol


instance ProcessState sv (Spot, v) where
  pSpot _ (s, _) = s



instance (StochasticVol p) => StochasticProcess p (Spot, Var) (Double, Double) where
  pEvolve' p dZ1dZ2 dt = do
    (s0, v0)   <- stateVariables
    t          <- time
    let (dW1, dW2) = svCorrelate p dZ1dZ2
        vol        = svGamma p v0
        u          = (drift (svDrift p) t dt - 0.5 * vol * vol) * coerce dt
        s'         = Spot $ coerce s0 * exp (u + vol * sqrt(coerce dt) * dW1)
        mu_v       = svVarianceDrift p v0
        sigma_v    = svVarianceVol p v0
        v'         = Var $ mu_v * coerce dt + sigma_v * dW2 * sqrt(coerce dt)
    put (t + dt, (s', v'))
    return s'


oneTrajectory :: (MonadRandom m, Distribution d b, StochasticProcess p s b) => p -> s -> d b -> [YearFrac] -> m [Spot]
oneTrajectory p s0 d ts = let trajectory = mapM (pEvolve p d) dts
                              dts        = zipWith (-) (tail ts) ts
                          in evalStateT (runSP trajectory) (0, s0)

oneTrajectory' :: (Distribution d b, StochasticProcess p s b, MonadRandom m) => p -> s -> d b -> [YearFrac] -> m Spot
oneTrajectory' p s0 d ts = let trajectory = do
                                 mapM_ (pEvolve p d) dts
                                 pSpot p <$> stateVariables
                               dts        = zipWith (-) (tail ts) ts
                           in evalStateT (runSP trajectory) (0, s0)

trajectories :: (MonadRandom m, Distribution d b, StochasticProcess p s b ) => Int -> p -> s -> d b -> [YearFrac]-> m [[Spot]]
trajectories n p s d ts = replicateM n (oneTrajectory p s d ts)

trajectories' :: (Distribution d b, StochasticProcess p s b, MonadRandom m) =>Int -> p -> s -> d b -> [YearFrac] -> m [Spot]
trajectories' n p s d ts = replicateM n (oneTrajectory' p s d ts)

evolve :: (MonadRandom m, Distribution d b, StochasticProcess p s b) => p -> s -> d b -> [YearFrac] -> m [Spot]
evolve p s0 d ts = let trajectory = mapM (pEvolve p d) dts
                       dts        = zipWith (-) (tail ts) ts
                    in evalStateT (runSP trajectory) (0, s0)



-- |Geometric Brownian motion \( dS_t = rS_t dt + \sigma S_t dW\)
data GeometricBrownianMotion = GBM {
    gbDrift :: Double -- ^ Drift
  , gbDiff  :: Double -- ^ Vol
} deriving stock (Show)


instance {-# OVERLAPPABLE #-} StochasticProcess GeometricBrownianMotion Spot Double where
  pEvolve' GBM{..} dW (YearFrac dt) = do
    (Spot s0)     <- stateVariables
    (YearFrac t0) <- time
    let s' = s0 * exp ((gbDrift - 0.5 * gbDiff * gbDiff) * dt + gbDiff * dW * sqrt dt)
    put (YearFrac (t0 + dt), Spot s')
    return $ Spot s'




data HestonModel = HestonModel
  { _drift :: ProcessDrift
  , _kappa :: Kappa
  , _theta :: Theta
  , _sigma :: Sigma
  , _rho   :: Rho
  }

instance StochasticVol HestonModel where
  svGamma _ (Var v)                         = if v >= 0 then sqrt v else {- use refelction -} sqrt (-v)
  svDrift HestonModel{..}                   = _drift
  svCorrelate HestonModel{..} (dB1, dB2)    = (dB1, coerce _rho * dB1 + sqrt(1 - coerce (_rho*_rho)) * dB2)
  svVarianceDrift HestonModel{..} (Var v)   = coerce _kappa * (coerce _theta - v)
  svVarianceVol p@HestonModel{..} (Var v)   = let vol = svGamma p (Var v)
                                              in coerce _sigma * vol

correlated :: Rho -> (Double, Double) -> (Double, Double)
correlated (Rho rho) (z1, z2) = (z1, rho * z1 + sqrt(1 - rho * rho) * z2)
