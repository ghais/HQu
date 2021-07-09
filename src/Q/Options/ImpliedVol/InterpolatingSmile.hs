{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Q.Options.ImpliedVol.InterpolatingSmile where

import Q.Types
import Q.SortedVector (SortedVector)
import Numeric.LinearAlgebra (Vector)
import Q.Options.ImpliedVol.TimeSlice
import Q.Options.ImpliedVol.StrikeInterpolation
import Q.Interpolation
data InterpolatingSmile = StrikeSmile
  {
    smileForward :: Forward
  , smileTenor   :: YearFrac
  , smileStrikes :: SortedVector Strike
  , smileVols    :: Vector Vol
  , smileInterpolation :: StrikeInterpolation
  , smileExtrapolation :: StrikeExtrapolation
  , smileMinStrike :: Strike
  , smileMaxStrike :: Strike
  }

instance TimeSlice InterpolatingSmile Strike where
  totalVar smile@StrikeSmile{..} k = TotalVar $ scale smileTenor (sigma * sigma) where
    (Vol sigma) = impliedVol smile k

impliedVol StrikeSmile{..} = interpolateV smileInterpolation smileStrikes smileVols
