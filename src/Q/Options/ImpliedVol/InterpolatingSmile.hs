{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Q.Options.ImpliedVol.InterpolatingSmile where

import           Q.Interpolation
import           Q.Options.ImpliedVol.StrikeInterpolation
import           Q.Options.ImpliedVol.TimeSlice
import           Q.Types

import           Control.Lens ((^.))
import           Control.Lens.TH
import           Q.Options.ImpliedVol.StrikeSpace



data InterpolatingSmile x = StrikeSmile
  {
    _forward       :: Forward
  , _tenor         :: YearFrac
  , _vols          :: Interpolation x Vol
  , _extrapolation :: ExtrapolationMethod
  , _minStrike     :: x
  , _maxStrike     :: x
  }

makeLenses ''InterpolatingSmile


instance (StrikeSpace x) => TimeSlice (InterpolatingSmile x ) x  where
  totalVar smile = smileTotalVar smile

instance ImpliedDensity (InterpolatingSmile LogRelStrike) LogRelStrike where
  dW smile = dW (smileTotalVar smile)
  d2W smile = dW (smileTotalVar smile)
  impliedDensity smile = impliedDensity (smileTotalVar smile)



smileImpliedVol :: forall x. (StrikeSpace x) => InterpolatingSmile x -> x -> Vol
smileImpliedVol smile x | x <= smile ^. minStrike = extrapolateSmall (smile ^. extrapolation)
                   | x >= smile ^. maxStrike = extrapolateLarge (smile ^. extrapolation)
                   | otherwise              = interpolate (smile ^. vols) x
  where extrapolateSmall Constant = snd $ (xMin (smile ^. vols)::(x, Vol))
        extrapolateLarge Constant = snd $ (xMax (smile ^. vols)::(x, Vol))


smileTotalVar :: forall x. (StrikeSpace x) => InterpolatingSmile x -> x -> TotalVar
smileTotalVar smile x = volToTotalVar (smileImpliedVol smile x) (smile ^. tenor)
