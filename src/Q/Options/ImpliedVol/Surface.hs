{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE BangPatterns #-}

module Q.Options.ImpliedVol.Surface
  (
    Surface(..)
  , StrikeSpace(..)
  , VolSurface(..)
  , ConstVolSurface(..)
  , fwdTotalVarKT
  , getTenor
  , surfaceForwardCurve
  )

where

import qualified Data.Map as M

import           Data.Maybe (fromJust, fromMaybe)
import qualified Q.Options.Bachelier as Bacherlier
import qualified Q.Options.Black76 as B76
import           Q.Options.ImpliedVol (VolShift (VolShift), VolType (Normal, ShiftedLogNormal))
import           Q.Options.ImpliedVol.TimeInterpolation (TimeInterpolation (..))
import           Q.Options.ImpliedVol.TimeSlice (TimeSlice (totalVar))
import           Q.SortedVector (SortedVector, maxElement, minElement)

import           Q.Options (Valuation)
import           Q.TermStructures (ForwardCurveTermStructure (..))
import           Q.Types (DF (..), Forward (..), OptionType,
                          Rate (Rate), Spot, Strike (..), TotalVar (..), Vol (..), YearFrac (..),
                          totalVarToVol, volToTotalVar)

import Q.Options.ImpliedVol.StrikeSpace ( StrikeSpace(..) )
import Control.Lens
import Data.Coerce

class (StrikeSpace k) => VolSurface s k where
  surfaceTotalVarKT :: s -> k -> YearFrac -> TotalVar
  surfaceTotalVarKT s k t = volToTotalVar t (surfaceVolKT s k t)

  surfaceVolKT :: s -> k -> YearFrac -> Vol
  surfaceVolKT s k t = totalVarToVol t (surfaceTotalVarKT s k t)

  surfacePremium :: s -> OptionType  -> k -> Spot -> Forward -> YearFrac -> Valuation
  surfacePremium s cp x spot f t = let b76 = B76.Black76 f df t vol
                              in B76.euOption b76 cp k
    where
          k    = strikeSpaceToCash k t spot f
          df   = 1
          vol  = surfaceVolKT s x t


newtype ConstVolSurface = ConstVolSurface Vol

instance (StrikeSpace k) => VolSurface ConstVolSurface k where
  surfaceTotalVarKT (ConstVolSurface vol) _ t = volToTotalVar t vol

-- | Implied volatility surface where the strikes are in the space of 'k' and
-- implied volatility time slice is 'v'.
data Surface v k = Surface
  {
    _surfaceSpot              :: Spot                   -- ^ Spot.
  , _surfaceTenors            :: SortedVector YearFrac  -- ^ Ordered list of tenors.
  , _surfaceForwardCurve      :: YearFrac -> Forward     -- ^ The forward curve.
  , _surfaceSmiles            :: M.Map YearFrac v       -- ^ Map from tenor to 'TimeSlice'
  , _surfaceTimeInterpolation :: TimeInterpolation      -- ^ Method of interpolation between tenors.
  , _surfaceType              :: VolType                -- ^ The type of surface.
  }

makeLenses ''Surface


surfaceAtmTotalVar :: forall v k . (TimeSlice v k) =>  Surface v k -> YearFrac -> TotalVar
surfaceAtmTotalVar surface t | t <= t0    = totalVar (smile t0)  atmf * coerce (t/t0)
                             | t >= tmax  = totalVar (smile tmax)  atmf * coerce (t/tmax)
                             | otherwise = let (t1, smile1)  = fromJust $ M.lookupLE t (surface ^. surfaceSmiles)
                                               (t2, smile2)  = fromJust $ M.lookupGE t (surface ^. surfaceSmiles)
                                               f1            = (surface ^. surfaceForwardCurve) t1
                                               f2            = (surface ^. surfaceForwardCurve) t2
                                               atmf1         = atmfStrike @k t1 s f1
                                               atmf2         = atmfStrike @k t2 s f2 :: k
                                               (TotalVar v1) = totalVar smile1 atmf1
                                               (TotalVar v2) = totalVar smile2 atmf2
                                           in TotalVar $ linearInterpolate (t1, v1) (t2, v2) t
  where t0      = minElement (surface ^. surfaceTenors)
        tmax    = maxElement (surface ^. surfaceTenors)
        smiles  = surface ^. surfaceSmiles
        smile t' = fromMaybe undefined (smiles ^.at t')
        s       = surface ^. surfaceSpot
        f       = (surface ^. surfaceForwardCurve) t0
        atmf :: k
        atmf    = atmfStrike t s f

instance forall v k. (StrikeSpace k, TimeSlice v k, Show k) => VolSurface (Surface v k) Strike where
  surfaceTotalVarKT s@Surface{..} k t = let x :: k
                                            x = cashToStrikeSpace k t _surfaceSpot (_surfaceForwardCurve t)
                                        in surfaceTotalVarKT s x t



instance ForwardCurveTermStructure (Surface v k) where
  tsForwardT = _surfaceForwardCurve
  tsSpot     = _surfaceSpot

instance  forall v k. (StrikeSpace k, TimeSlice v k, Show k) =>  VolSurface (Surface v k) k where
  surfaceTotalVarKT s@Surface{..} k t | t <= minElement _surfaceTenors =
                                          extrapolateTotalVarFrom (minElement _surfaceTenors) s k t
                                      | t >= maxElement _surfaceTenors =
                                          extrapolateTotalVarFrom (maxElement _surfaceTenors) s k t
                                      | otherwise =
                                          timeInterpolate _surfaceTimeInterpolation s  k t

fwdTotalVarKT :: (VolSurface s k) => s -> k -> YearFrac -> k -> YearFrac -> TotalVar
fwdTotalVarKT surface k1 t1 k2 t2 = TotalVar $ (totalVarKT2 - totalVarKT1) / (unYearFrac t2 - unYearFrac t1)
  where (TotalVar totalVarKT1) = surfaceTotalVarKT surface k1 t1
        (TotalVar totalVarKT2) = surfaceTotalVarKT surface k2 t2



slnShift :: Surface v k -> VolShift
slnShift surface = case surface ^. surfaceType of
  (ShiftedLogNormal shift) -> shift
  _                        -> VolShift 0

extrapolateTotalVarFrom :: forall v k. ( TimeSlice v k) =>YearFrac -> Surface v k -> k -> YearFrac -> TotalVar
extrapolateTotalVarFrom t0 Surface{..} x t = (totalVar (_surfaceSmiles M.! t0) x') / (coerce  t0) * (coerce t) where
  f0          = _surfaceForwardCurve t0
  spot        = _surfaceSpot
  k'          = strikeSpaceToCash x t0 spot f0
  x'          = cashToStrikeSpace k' t0 spot f0::k
  !mydata      = (x, f0, spot, k', x' )



timeInterpolate :: TimeSlice v k => TimeInterpolation -> Surface v k -> k -> YearFrac -> TotalVar
timeInterpolate LinearInVol Surface{..} x t =
  let (t1, smile1) = fromJust $ M.lookupLE t _surfaceSmiles
      (t2, smile2) = fromJust $ M.lookupGT t _surfaceSmiles
      (Vol vol1)         = totalVarToVol t1 (totalVar smile1 x)
      (Vol vol2)         = totalVarToVol t2 (totalVar smile2 x)
  in volToTotalVar t (Vol $ linearInterpolate (t1, vol1) (t2, vol2) t)

timeInterpolate LinearInTotalVar Surface{..} x t =
  let (t1, smile1) = fromJust $ M.lookupLE t _surfaceSmiles
      (t2, smile2) = fromJust $ M.lookupGT t _surfaceSmiles
      (TotalVar tv1)         = totalVar smile1 x
      (TotalVar tv2)         = totalVar smile2 x
  in TotalVar $ linearInterpolate (t1, tv1) (t2, tv2) t

euOption :: VolType -> Strike -> Forward -> DF -> YearFrac -> Vol -> Valuation
euOption Normal k f (DF df) (YearFrac t) vol =
  let r = Rate $ -log df / t
      bacherlier = Bacherlier.Bachelier (YearFrac t) f r vol
  in Bacherlier.eucall bacherlier k

euOption _ k f df t vol =
  let b76 = B76.Black76 f df t vol
  in B76.eucall b76 k

linearInterpolate :: (YearFrac, Double) -> (YearFrac, Double) -> YearFrac -> Double
linearInterpolate (YearFrac t1, v1) (YearFrac t2, v2) (YearFrac t) =
  v1  + (v2 - v1)*(t - t1) / (t2 - t1)


getTenor :: Surface v k -> YearFrac -> Maybe v
getTenor surface t = M.lookup t (surface ^. surfaceSmiles)
