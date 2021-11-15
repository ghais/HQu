{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
module Q.Stochastic.Process
        where
import Control.Monad
import Control.Monad.State
import Data.RVar
import Data.Random


import Q.Types


import Data.Coerce (coerce)





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
  abs = fmap abs
  signum = fmap signum
  fromInteger x = return $ fromInteger x



newtype StochasticProcessMonad s m a = PM {runSP :: StateT (YearFrac, s) m a} deriving newtype (Functor, Applicative, Monad, MonadTrans , MonadState (YearFrac, s))


model :: Monad m => StochasticProcessMonad s m s
model = gets snd
time :: Monad m => StochasticProcessMonad s m YearFrac
time  = gets fst

class HasSpot a where
  spot :: a -> Spot


-- |A stochastic process of the form \(dX_t = \mu(X_t, t)dt + \sigma(S_t, t)dB_t \)
class StochasticProcess p s b | p -> s, p -> b where
  pEvolve :: (MonadRandom m, Distribution d b) => p -> YearFrac -> (d b) -> StochasticProcessMonad (t, s) m s


-- | \(dln(S(t)) =  (\mu(t) - 0.5 * L^2(S, t) * \gamma^2(v)) * dt + L(S, t) * \gamma(v) * dW1(t) \)
-- |
-- | \(dv(t) = \mu_v(V) dt + \sigma_v(V)*dW2(t))
class SLVProcess p where
  slvLeverage      :: p -> Spot -> YearFrac -> YearFrac -> Vol
  slvGamma         :: p -> Vol -> Vol
  slvDrift         :: p -> YearFrac -> YearFrac -> Rate
  slvCorrelate     :: p -> (Double, Double) -> (Double, Double)
  -- | mu_v and sigma_v of the vol process.
  slvVolDriftSigma :: p -> Vol -> (Double, Double)


class StochasticVol p where
  svGamma :: p -> Vol -> Vol
  svDrift :: p -> YearFrac -> YearFrac -> Rate
  svCorrelate :: p -> (Double, Double) -> (Double, Double)
  svVolDriftSigma :: p -> Vol -> (Double, Double)
instance (StochasticVol p) => SLVProcess p (Spot, Vol) (Double, Double) where
  slvLeverage _ _ _ _ = 1
  slvGamma            = svGamma
  slvDrift            = svDrift
  slvCorrelate        = svCorrelate
  slvVolDriftSigma    = svVolDriftSigma




instance (StochasticVol p) => StochasticProcess p (Spot, Vol) (Double, Double) where
  pEvolve p (YearFrac dt) d = do
    (t0, (Spot s0, Vol v0)) <- get
    (dW1, dW2) <- svCorrelate p <$> lift (sample d)
    let (Vol vol)     = svGamma p v0
        (Rate u)        = svDrift p t dt
        s'              = s0 * exp (u + vol * sqrt dt * dW1)
        (mu_v, sigma_v) = svVolDriftSigma p v0
        v'              = v0 + mu_v * dt + sigma_v * dW2 * sqrt dt
    put (Spot s', Vol v')



vol :: (StochasticLocalVol p s b) => p -> Spot -> Vol -> YearFrac -> YearFrac -> Double
vol p s v t dt = coerce $ slvLeverage p s t dt $*$ slvGamma p v

drift :: (StochasticLocalVol p s b) => p -> Spot -> Vol -> YearFrac -> YearFrac -> Double
drift p s v t dt = let sigma    = vol p s v t dt
                       (Rate u) = slvDrift p t dt
                   in  u - (sigma**2)/2

{-- oneTrajectory :: (MonadRandom m, Distribution d b, StochasticProcess p s b) => p -> s -> d b -> [YearFrac] -> m [p]
oneTrajectory s0 d ts = evalStateT (runSP trajectory) (0, s0)
   where trajectory = mapM (`pEvolve` d) ts


trajectories :: (MonadRandom m, Distribution d b, StochasticProcess p s b) => Int -> p -> d b ->  [YearFrac] -> m [[p]]
trajectories n p d ts = replicateM n (oneTrajectory p d ts)

-- |Geometric Brownian motion \( dS_t = rS_t dt + \sigma S_t dW\)
data GeometricBrownianMotion = GBM {
    gbDrift :: Double -- ^ Drift
  , gbDiff  :: Double -- ^ Vol
  , gbS0    :: Double -- ^ Current value
} deriving stock (Show)

instance HasSpot  GeometricBrownianMotion where
  spot GBM{..} = Spot $ gbS0
evolveGBM :: (YearFrac, GeometricBrownianMotion) -> YearFrac -> Double -> GeometricBrownianMotion
evolveGBM (YearFrac t, GBM{..}) (YearFrac t') dW = GBM gbDrift gbDiff s'
  where s' = gbS0 * exp ((gbDrift - 0.5 * gbDiff * gbDiff) * dt + gbDiff * dW * sqrt dt)
        dt = t' - t
data HestonModel = HestonModel {
    _s_0    :: Double
  , _drift  :: YearFrac -> Rate
  , _lambda :: Double
  , _nu     :: Double
  , _eta    :: Double
  , _rho    :: Double
  , _nu_t   :: Double
  , _x_t    :: Double -- ^ \( log (S_t/S_0) \)
  }

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
