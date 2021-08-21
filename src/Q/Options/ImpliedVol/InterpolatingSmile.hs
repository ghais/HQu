{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Q.Options.ImpliedVol.InterpolatingSmile where

import Q.Types
import Q.Options.ImpliedVol.TimeSlice
import Q.Options.ImpliedVol.StrikeInterpolation
import Q.Interpolation
import qualified Data.SortedList as SortedList
data InterpolatingSmile = StrikeSmile
  {
    smileForward :: Forward
  , smileTenor   :: YearFrac
  , smileVols    :: SortedList.SortedList (Strike, Vol)
  , smileInterpolation :: InterpolationMethod
  , smileExtrapolation :: ExtrapolationMethod
  , smileMinStrike :: Strike
  , smileMaxStrike :: Strike
  }

instance TimeSlice InterpolatingSmile Strike where
  totalVar smile@StrikeSmile{..} k = TotalVar $ scale smileTenor (sigma * sigma) where
    (Vol sigma) = impliedVol smile k

impliedVol :: Interpolator StrikeInterpolation x y => InterpolatingSmile -> x -> y
impliedVol StrikeSmile{..} = undefined
