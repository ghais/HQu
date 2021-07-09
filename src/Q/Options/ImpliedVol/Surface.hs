{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Q.Options.ImpliedVol.Surface
  (
    Surface(..)
  , totalVarKT
  , fwdTotalVarKT
  , volKT)

where

import qualified Data.Map                                 as M
import qualified Data.Map.Strict                          as M
import           Data.Maybe                               (fromJust)
import           Numeric.LinearAlgebra                    hiding (maxElement,
                                                           minElement)
import qualified Q.Options.Bachelier                      as Bacherlier
import qualified Q.Options.Black76                        as B76
import           Q.Options.ImpliedVol
import           Q.Options.ImpliedVol.InterpolatingSmile
import           Q.Options.ImpliedVol.StrikeInterpolation
import           Q.Options.ImpliedVol.TimeInterpolation
import           Q.Options.ImpliedVol.TimeSlice
import           Q.SortedVector
import           Q.Types

-- | Implied volatility surface where the strikes are in the space of 'k' and
-- implied volatility time slice is 'v'.
data Surface v k = Surface
  {
    surfaceSpot              :: Spot                   -- ^ Spot.
  , surfaceTenors            :: SortedVector YearFrac  -- ^ Ordered list of tenors.
  , surfaceForwardCurve      :: YearFrac -> Forward    -- ^ The forward curve.
  , surfaceDiscountCurve     :: YearFrac -> DF         -- ^ The discount curve.
  , surfaceAtmTotalVar       :: YearFrac -> TotalVar   -- ^ A spline of the at the money total variance.
  , surfaceVols              :: M.Map YearFrac v       -- ^ Map from tenor to 'TimeSlice'
  , surfaceTimeInterpolation :: TimeInterpolation      -- ^ Method of interpolation between tenors.
  , surfaceType              :: VolType                -- ^ The type of surface.
  }

totalVarKT :: (StrikeSpace k, TimeSlice v k) => Surface v k -> Strike -> YearFrac -> TotalVar
totalVarKT surface@Surface{..} k t | t <= minElement surfaceTenors =
                                       extrapolateTotalVarFrom (minElement surfaceTenors) surface k t
                                   | t >= maxElement surfaceTenors =
                                       extrapolateTotalVarFrom (maxElement surfaceTenors) surface k t
                                   | otherwise =
                                       timeInterpolate surfaceTimeInterpolation surface k t


volKT :: (StrikeSpace k, TimeSlice v k) => Surface v k -> Strike -> YearFrac ->  Vol
volKT surface k t = totalVarToVol (totalVarKT surface k t) t

fwdTotalVarKT :: ( StrikeSpace k, TimeSlice v k) => Surface v k -> Strike -> YearFrac -> Strike -> YearFrac -> TotalVar
fwdTotalVarKT surface@Surface{..} k1 t1 k2 t2 = TotalVar $ (totalVarKT2 - totalVarKT1) / (unYearFrac t2 - unYearFrac t1)
  where (TotalVar totalVarKT1) = totalVarKT surface k1 t1
        (TotalVar totalVarKT2) = totalVarKT surface k2 t2


class StrikeSpace k where
  strikeSpaceToCash :: k -> YearFrac -> Spot -> Forward -> Vol -> VolShift -> Strike
  cashToStrikeSpace :: Strike -> YearFrac -> Spot -> Forward -> Vol -> VolShift -> k

instance StrikeSpace Strike where
  strikeSpaceToCash x _ _ _ _ _ = x
  cashToStrikeSpace k _ _ _ _ _ = k


instance StrikeSpace AbsRelStrike where
  strikeSpaceToCash (AbsRel x) _ _ (Forward f) _ _ = Strike $ x + f
  cashToStrikeSpace (Strike k) _ _ (Forward f) _ _ = AbsRel $ k - f

instance StrikeSpace LogRelStrike where
  strikeSpaceToCash (LogRel x) _ _ (Forward f) _ _ = Strike $ f * exp x
  cashToStrikeSpace (Strike k) _ _ (Forward f) _ _ = LogRel $ log $ k - f

instance StrikeSpace MoneynessForwardStrike  where
  strikeSpaceToCash (MoneynessForward x) (YearFrac t) _ (Forward f) (Vol atmVol) _ =
    Strike $ x * sqrt t * atmVol + f

  cashToStrikeSpace (Strike k) (YearFrac t) _ (Forward f) (Vol atmVol) _ =
    MoneynessForward $ (k - f) / (atmVol * sqrt t)

instance StrikeSpace LogMoneynessForwardStrike  where
  strikeSpaceToCash (LogMoneynessForward x) (YearFrac t) _ (Forward f) (Vol atmVol) (VolShift slnShift) =
    Strike $ (f + slnShift) * exp (x * (sqrt t) * atmVol) - slnShift

  cashToStrikeSpace (Strike k) (YearFrac t)  _ (Forward f) (Vol atmVol) (VolShift slnShift) =
    LogMoneynessForward $ (log ((k - slnShift) / (f + slnShift))) / (atmVol * sqrt t)

instance StrikeSpace LogMoneynessSpotStrike  where
  strikeSpaceToCash (LogMoneynessSpot x) (YearFrac t) (Spot s) _ (Vol atmVol) (VolShift slnShift) =
    Strike $ (s + slnShift) * exp (x * (sqrt t) * atmVol) - slnShift

  cashToStrikeSpace (Strike k) (YearFrac t)  (Spot s) _ (Vol atmVol) (VolShift slnShift) =
    LogMoneynessSpot $ (log ((k - slnShift) / (s + slnShift))) / (atmVol * sqrt t)

instance StrikeSpace MoneynessSpotStrike  where
  strikeSpaceToCash (MoneynessSpot x) (YearFrac t) (Spot s) _ (Vol atmVol) _ =
    Strike $ x * (sqrt t) * atmVol + s

  cashToStrikeSpace (Strike k) (YearFrac t) (Spot s) _ (Vol atmVol) _ =
    MoneynessSpot $ (k - s) / (atmVol * sqrt t)


slnShift Surface{..} = case surfaceType of
  (ShiftedLogNormal shift) -> shift
  _                        -> VolShift 0
extrapolateTotalVarFrom :: forall v k. (StrikeSpace k, TimeSlice v k) => YearFrac -> Surface v k -> Strike -> YearFrac -> TotalVar
extrapolateTotalVarFrom t0 surface@Surface{..} k t = let
  f0          = surfaceForwardCurve t0
  atmVol0     = totalVarToVol (surfaceAtmTotalVar t0) t0
  f           = surfaceForwardCurve t
  spot        = surfaceSpot
  atmTotalVar = surfaceAtmTotalVar t
  atmVol      = totalVarToVol atmTotalVar t
  x           = cashToStrikeSpace k t spot f atmVol (slnShift surface)::k
  k'          = strikeSpaceToCash x t0 spot f0 atmVol0 (slnShift surface)
  x'          = cashToStrikeSpace k' t0 spot f0 atmVol (slnShift surface)::k
  in totalVar (surfaceVols M.! t0) x'


timeInterpolate :: forall v k. (StrikeSpace k, TimeSlice v k) => TimeInterpolation -> Surface v k -> Strike -> YearFrac -> TotalVar
timeInterpolate Gatheral surface@Surface{..} k11 t =
  let (t1, smile1) = fromJust $ M.lookupLE t surfaceVols
      (t2, smile2) = fromJust $ M.lookupGE t surfaceVols
      (TotalVar thetaT)  = surfaceAtmTotalVar t
      (TotalVar thetaT1) = surfaceAtmTotalVar t1
      (TotalVar thetaT2) = surfaceAtmTotalVar t2
      alphaT  = (sqrt thetaT2 - sqrt thetaT1 ) / (sqrt thetaT1 - sqrt thetaT)
      atmVol  = totalVarToVol (TotalVar thetaT) t
      (Forward f)  = surfaceForwardCurve t
      (Forward f1) = surfaceForwardCurve t1
      (Forward f2) = surfaceForwardCurve t2
      df  = surfaceDiscountCurve t
      df1 = surfaceDiscountCurve t1
      df2 = surfaceDiscountCurve t2
      k1      = k11 $*$ (f1 / f)
      k2      = k11 $*$ (f2 / f)
      x1      = cashToStrikeSpace k1 t1 surfaceSpot (Forward f1) atmVol (slnShift surface)::k
      x2      = cashToStrikeSpace k2 t2 surfaceSpot (Forward f2) atmVol (slnShift surface)::k
      vol1 = totalVarToVol (totalVar smile1 x1) t1
      vol2 = totalVarToVol (totalVar smile2  x2) t2
      (Premium premium1) = vPremium $ euOption surfaceType k1 f1 df1 t1 vol1
      (Premium premium2) = vPremium $ euOption surfaceType k1 f2 df2 t1 vol1
      premiumT  = Premium $ (alphaT * premium1 $/$ k1 + (1- alphaT)* premium2 $/$ k2) $*$ k11
      x1 :: k
      x2 :: k
  in if surfaceType == Normal then
       volToTotalVar (euImpliedVol Normal Call (Forward f) k11 t df premiumT) t
     else
       volToTotalVar (euImpliedVol LogNormal Call (Forward f) k11 t df premiumT) t

timeInterpolate LinearInVol surface@Surface{..} k t =
  let f = surfaceForwardCurve t
      atmVol = totalVarToVol (surfaceAtmTotalVar t) t
      x      = cashToStrikeSpace k t surfaceSpot f atmVol (slnShift surface)::k
      (t1, smile1) = fromJust $ M.lookupLE t surfaceVols
      (t2, smile2) = fromJust $ M.lookupGE t surfaceVols
      (Vol vol1)         = totalVarToVol (totalVar smile1 x) t1
      (Vol vol2)         = totalVarToVol (totalVar smile2 x) t2
  in volToTotalVar (Vol $ linearInterpolate (t1, vol1) (t2, vol2) t) t

timeInterpolate LinearInTotalVar surface@Surface{..} k t =
  let f = surfaceForwardCurve t
      atmVol = totalVarToVol (surfaceAtmTotalVar t) t
      x      = cashToStrikeSpace k t surfaceSpot f atmVol (slnShift surface)::k
      (t1, smile1) = fromJust $ M.lookupLE t surfaceVols
      (t2, smile2) = fromJust $ M.lookupGE t surfaceVols
      (TotalVar tv1)         = totalVar smile1 x
      (TotalVar tv2)         = totalVar smile2 x
  in TotalVar $ linearInterpolate (t1, tv1) (t2, tv2) t

euOption Normal k f (DF df) (YearFrac t) vol =
  let r = Rate $ -(log df) / t
      bacherlier = Bacherlier.Bachelier (Forward f) r vol
  in Bacherlier.eucall bacherlier (YearFrac t) k

euOption _ k f df t vol =
  let b76 = B76.Black76 (Forward f) df t vol
  in B76.eucall b76 k

linearInterpolate (YearFrac t1, v1) (YearFrac t2, v2) (YearFrac t) =
  v1 + (v2 - v1)*(t - t1) / (t2 - t1)
