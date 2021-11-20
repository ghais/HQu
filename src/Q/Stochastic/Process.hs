{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE UndecidableInstances   #-}

module Q.Stochastic.Process
        where
import           Control.Monad.State
import           Data.RVar
import           Data.Random


import Q.Types
    ( Var(..),
      Spot(..),
      YearFrac(YearFrac),
      Rate(..),
      Theta(..),
      Rho(..),
      Sigma(..),
      Kappa(..) )
import Data.Coerce

newtype ProcessDrift  = ProcessDrift {unDrift :: YearFrac -> YearFrac -> Double}


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




newtype StochasticProcessMonad s m a = PM {runSP :: StateT (YearFrac, s) m a} deriving newtype (Functor, Applicative, Monad, MonadTrans , MonadState (YearFrac, s))



stateVariables :: Monad m => StochasticProcessMonad s m s
stateVariables = gets snd


time :: Monad m => StochasticProcessMonad s m YearFrac
time  = gets fst

class HasSpot a where
  spot :: a -> Spot


-- |A stochastic process of the form \(dX_t = \mu(X_t, t)dt + \sigma(S_t, t)dB_t \)
class StochasticProcess p s b where
  pEvolve :: (MonadRandom m, Distribution d b) => p -> d b -> YearFrac -> StochasticProcessMonad s m Spot



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



instance (StochasticVol p) => SLVProcess p where
  slvLeverage _ _ _ _ = 1
  slvGamma            = svGamma
  slvDrift            = svDrift
  slvCorrelate        = svCorrelate
  slvVarianceDrift    = svVarianceDrift
  slvVarianceVol      = svVarianceVol




instance (StochasticVol p) => StochasticProcess p (Spot, Var) (Double, Double) where
  pEvolve p d dt = do
    (s0, v0)   <- stateVariables
    t          <- time
    (dW1, dW2) <- svCorrelate p <$> lift (sample d)
    let vol     = svGamma p v0
        u       = ((drift (svDrift p) t dt) - (0.5 * vol * vol)) * coerce dt
        s'      = Spot $ coerce s0 * exp (u + vol * sqrt(coerce dt) * dW1)
        mu_v    = svVarianceDrift p v0
        sigma_v = svVarianceVol p v0
        v'      = Var $ mu_v * coerce dt + sigma_v * dW2 * sqrt(coerce dt)
    put (t + dt, (s', v'))
    return s'


oneTrajectory :: (MonadRandom m, Distribution d b, StochasticProcess p s b) => p -> s -> d b -> [YearFrac] -> m [Spot]
oneTrajectory p s0 d ts = let trajectory = mapM (pEvolve p d) dts
                              dts        = zipWith (-) (tail ts) ts
                          in evalStateT (runSP trajectory) (0, s0)





trajectories :: (MonadRandom m, Distribution d b, StochasticProcess p s b ) => Int -> p -> s -> d b -> [YearFrac]-> [m [Spot]]
trajectories n p d ts = replicateM n (oneTrajectory p d ts)


-- |Geometric Brownian motion \( dS_t = rS_t dt + \sigma S_t dW\)
data GeometricBrownianMotion = GBM {
    gbDrift :: Double -- ^ Drift
  , gbDiff  :: Double -- ^ Vol
} deriving stock (Show)


instance StochasticProcess GeometricBrownianMotion Spot Double where
  pEvolve GBM{..} d (YearFrac dt) = do
    (Spot s0)     <- stateVariables
    (YearFrac t0) <- time
    dW <- lift (sample d)
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
{-
$(makeLenses ''HestonModel)

instance HasSpot  HestonModel where
  spot HestonModel{..} = Spot $ _s_0 * exp _x_t

mkHeston :: Spot -> (YearFrac -> Rate) -> Nu -> Lambda -> Eta -> Rho -> Nu -> HestonModel
mkHeston s0 r nu lambda eta rho nu_t = HestonModel (coerce s0) r (coerce lambda) (coerce nu) (coerce eta) (coerce rho) (coerce nu_t) 0





evolveHeston :: (YearFrac, HestonModel) -> YearFrac -> (Double, Double) -> HestonModel
evolveHeston (YearFrac t, heston@HestonModel{..}) (YearFrac dt) (dW1, dW2) =
  let (dZ1, dZ2) = correlate _rho dW1 dW2
      nu_t' = ((sqrt _nu_t) + 0.5 * _eta * (sqrt dt) * dZ2)**2 - _lambda * (_nu_t - _nu) * dt - 0.25 *  _eta * _eta * dt
      x_t'  = _x_t + (coerce (_drift (YearFrac t)) - 0.5 *  _nu_t) * dt + sqrt (_nu_t * dt) * dZ1
      in (heston & nu_t .~ nu_t' & x_t .~ x_t')
  where correlate rho b1 b2 = (b1, rho * b1 + sqrt(1 - rho * rho) * b2)

instance StochasticProcess HestonModel (Double, Double) where
  pDt _ = return $ 1/365
  pEvolve_dt dt dW = do
    (t, heston) <- get
    let heston' = evolveHeston (t, heston) dt dW
    put (t + dt, heston')


instance StochasticProcess GeometricBrownianMotion Double where
  pDt _ = return $ 1/360
  pEvolve t' b = do
    !dw <- lift $ sample b
    (!t, !gbm) <- get
    let gbm'  = evolveGBM (t, gbm) t' dw
    put (t', gbm')
    return gbm'
  pEvolve_dt dt dW = do
    !gbm <- model
    !t   <- time
    let gbm' = evolveGBM (t, gbm) dt dW
    put (t+dt, gbm')



--}
