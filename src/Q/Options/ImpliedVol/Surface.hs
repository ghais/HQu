{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Q.Options.ImpliedVol.Surface
  (
    Surface(..)
  , StrikeSpace(..)
  , VolSurface(..)
  , fwdTotalVarKT)

where

import qualified Data.Map as M

import           Data.Maybe (fromJust)
import qualified Q.Options.Bachelier as Bacherlier
import qualified Q.Options.Black76 as B76
import           Q.Options.ImpliedVol (VolShift (VolShift), VolType (Normal, ShiftedLogNormal))
import           Q.Options.ImpliedVol.TimeInterpolation (TimeInterpolation (..))
import           Q.Options.ImpliedVol.TimeSlice (TimeSlice (totalVar))
import           Q.SortedVector (SortedVector, maxElement, minElement)

import           Q.Options (Valuation)
import           Q.TermStructures (ForwardCurveTermStructure (..))
import           Q.Types (AbsRelStrike (..), DF (..), Forward (..), LogRelStrike (..), OptionType,
                          Rate (Rate), Spot, Strike (..), TotalVar (..), Vol (..), YearFrac (..),
                          totalVarToVol, volToTotalVar)


class (StrikeSpace k) => VolSurface s k where
  surfaceTotalVarKT :: s -> k -> YearFrac -> TotalVar
  surfaceTotalVarKT s k t = volToTotalVar (surfaceVolKT s k t) t

  surfaceVolKT :: s -> k -> YearFrac -> Vol
  surfaceVolKT s k t = totalVarToVol (surfaceTotalVarKT s k t) t

  surfacePremium :: s -> OptionType  -> k -> Spot -> Forward -> YearFrac -> Valuation 
  surfacePremium s cp x spot f t = let b76 = B76.Black76 f df t vol
                              in B76.euOption b76 cp k
    where 
          k    = strikeSpaceToCash k t spot f
          df   = 1
          vol  = surfaceVolKT s x t





-- | Implied volatility surface where the strikes are in the space of 'k' and
-- implied volatility time slice is 'v'.
data Surface v k = Surface
  {
    _surfaceSpot              :: Spot                   -- ^ Spot.
  , _surfaceTenors            :: SortedVector YearFrac  -- ^ Ordered list of tenors.
  , _surfaceForwardCurve      :: YearFrac -> Forward    -- ^ The forward curve.
  , _surfaceDiscountCurve     :: YearFrac -> DF         -- ^ The discount curve.
  , _surfaceAtmTotalVar       :: YearFrac -> TotalVar   -- ^ A spline of the at the money total variance.
  , _surfaceVols              :: M.Map YearFrac v       -- ^ Map from tenor to 'TimeSlice'
  , _surfaceTimeInterpolation :: TimeInterpolation      -- ^ Method of interpolation between tenors.
  , _surfaceType              :: VolType                -- ^ The type of surface.
  }

instance forall v k. (StrikeSpace k, TimeSlice v k) => VolSurface (Surface v k) Strike where
  surfaceTotalVarKT s@Surface{..} k t = let x :: k
                                            x = cashToStrikeSpace k t _surfaceSpot (_surfaceForwardCurve t)
                                        in surfaceTotalVarKT s x t
  
instance ForwardCurveTermStructure (Surface v k) where
  tsForwardT = _surfaceForwardCurve
  tsSpot     = _surfaceSpot

instance  forall v k. (StrikeSpace k, TimeSlice v k) =>  VolSurface (Surface v k) k where
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


class StrikeSpace k where
  strikeSpaceToCash :: k -> YearFrac -> Spot -> Forward -> Strike
  cashToStrikeSpace :: Strike -> YearFrac -> Spot -> Forward ->  k

instance StrikeSpace Strike where
  strikeSpaceToCash x _ _ _ = x
  cashToStrikeSpace k _ _ _ = k


instance StrikeSpace AbsRelStrike where
  strikeSpaceToCash (AbsRel x) _ _ (Forward f) = Strike $ x + f
  cashToStrikeSpace (Strike k) _ _ (Forward f) = AbsRel $ k - f

instance StrikeSpace LogRelStrike where
  strikeSpaceToCash (LogRel x) _ _ (Forward f) = Strike $ f * exp x
  cashToStrikeSpace (Strike k) _ _ (Forward f) = LogRel $ log $ k - f




slnShift Surface{..} = case _surfaceType of
  (ShiftedLogNormal shift) -> shift
  _                        -> VolShift 0
extrapolateTotalVarFrom :: forall v k. (StrikeSpace k, TimeSlice v k) => YearFrac -> Surface v k -> k -> YearFrac -> TotalVar
extrapolateTotalVarFrom t0 surface@Surface{..} x t = let
  f0          = _surfaceForwardCurve t0
  atmVol0     = totalVarToVol (_surfaceAtmTotalVar t0) t0
  f           = _surfaceForwardCurve t
  spot        = _surfaceSpot
  atmTotalVar = _surfaceAtmTotalVar t
  atmVol      = totalVarToVol atmTotalVar t
  k'          = strikeSpaceToCash x t0 spot f0
  x'          = cashToStrikeSpace k' t0 spot f0::k
  in totalVar (_surfaceVols M.! t0) x'


timeInterpolate :: forall v k. (StrikeSpace k, TimeSlice v k) => TimeInterpolation -> Surface v k -> k -> YearFrac -> TotalVar
timeInterpolate Gatheral surface@Surface{..} k t = undefined

timeInterpolate LinearInVol surface@Surface{..} x t =
  let f = _surfaceForwardCurve t
      atmVol = totalVarToVol (_surfaceAtmTotalVar t) t
      (t1, smile1) = fromJust $ M.lookupLE t _surfaceVols
      (t2, smile2) = fromJust $ M.lookupGE t _surfaceVols
      (Vol vol1)         = totalVarToVol (totalVar smile1 x) t1
      (Vol vol2)         = totalVarToVol (totalVar smile2 x) t2
  in volToTotalVar (Vol $ linearInterpolate (t1, vol1) (t2, vol2) t) t

timeInterpolate LinearInTotalVar surface@Surface{..} x t =
  let f = _surfaceForwardCurve t
      atmVol = totalVarToVol (_surfaceAtmTotalVar t) t
      (t1, smile1) = fromJust $ M.lookupLE t _surfaceVols
      (t2, smile2) = fromJust $ M.lookupGE t _surfaceVols
      (TotalVar tv1)         = totalVar smile1 x
      (TotalVar tv2)         = totalVar smile2 x
  in TotalVar $ linearInterpolate (t1, tv1) (t2, tv2) t

euOption :: VolType -> Strike -> Forward -> DF -> YearFrac -> Vol -> Valuation
euOption Normal k f (DF df) (YearFrac t) vol =
  let r = Rate $ -(log df) / t
      bacherlier = Bacherlier.Bachelier (YearFrac t) f r vol
  in Bacherlier.eucall bacherlier k

euOption _ k f df t vol =
  let b76 = B76.Black76 f df t vol
  in B76.eucall b76 k

linearInterpolate (YearFrac t1, v1) (YearFrac t2, v2) (YearFrac t) =
  v1 + (v2 - v1)*(t - t1) / (t2 - t1)
