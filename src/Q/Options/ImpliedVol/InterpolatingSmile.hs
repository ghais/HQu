{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Q.Options.ImpliedVol.InterpolatingSmile where

import           Q.Interpolation (Interpolation, Interpolator (interpolate, xMax, xMin))
import           Q.Options.ImpliedVol.StrikeInterpolation (ExtrapolationMethod (..))
import           Q.Options.ImpliedVol.TimeSlice (ImpliedDensity (..), TimeSlice (..))
import           Q.Types (Alpha (Alpha), Beta (Beta), Forward, LogRelStrike, M (M), Rho (Rho),
                          Sigma (Sigma), TotalVar(..), Vol (..), YearFrac, totalVarToVol, volToTotalVar)

import           Control.Lens ((^.))
import           Control.Lens.TH (makeLenses)
import           Data.Coerce (coerce)
import qualified Data.Vector.Storable as V
import qualified Numeric.LevMar as LevMar
import           Q.Options.ImpliedVol.SVI (SVI (RSVI))
import           Q.Options.ImpliedVol.StrikeSpace (StrikeSpace)

import Numeric.LevMar (Constraints(lowerBounds), upperBounds)

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

class X x where
  foo :: x -> Double


newtype Y = Y Double

instance X Y where
  foo (Y y) = y + 1

instance (StrikeSpace x) => TimeSlice (InterpolatingSmile x) x  where
  totalVar smile = smileTotalVar smile

instance ImpliedDensity (InterpolatingSmile LogRelStrike) LogRelStrike where
  dW smile = dW (smileTotalVar smile)
  d2W smile = dW (smileTotalVar smile)
  impliedDensity smile = impliedDensity (smileTotalVar smile)



smileImpliedVol :: forall x. (StrikeSpace x) => InterpolatingSmile x -> x -> Vol
smileImpliedVol smile x | x <= smile ^. minStrike = extrapolateSmall (smile ^. extrapolation)
                   | x >= smile ^. maxStrike = extrapolateLarge (smile ^. extrapolation)
                   | otherwise              = interpolate (smile ^. vols) x
  where extrapolateSmall Constant = snd (xMin (smile ^. vols)::(x, Vol))
        extrapolateLarge Constant = snd (xMax (smile ^. vols)::(x, Vol))


smileTotalVar :: forall x. (StrikeSpace x) => InterpolatingSmile x -> x -> TotalVar
smileTotalVar smile x = volToTotalVar (smile ^. tenor) (smileImpliedVol smile x)


fitSVI :: InterpolatingSmile LogRelStrike -> [LogRelStrike] -> Either String SVI
fitSVI smile strikes = case LevMar.levmar model Nothing params samples 100 LevMar.defaultOpts constraints of
  Left err        -> Left $ show err
  Right (p, r, _) -> do
    Right $ toSVI p
  where
    t       = smile ^. tenor
    model :: LevMar.Params Double -> LevMar.Params Double
    model p = let svi   = toSVI p
                  ws    = map (totalVar svi) strikes
                  in (V.fromList (coerce  ws))
    params  = V.fromList [0.2, 0.1, 0, 0, 0.2]
    samples = V.fromList $ coerce $ map (totalVar smile) strikes
    constraints = LevMar.Constraints
      {
        lowerBounds=Just $ V.fromList [-0.5, 1e-5, -0.9999, -0.4, 1e-4]
      , upperBounds=Just $ V.fromList [3   , 5,  0.9999,  0.4, 5   ]
      , weights=Nothing
      , linearConstraints=Nothing
      }
    toSVI p = let alpha = coerce (p V.! 0)
                  beta  = coerce (p V.! 1)
                  rho   = coerce (p V.! 2)
                  m     = coerce (p V.! 3)
                  sigma = coerce (p V.! 4)
              in RSVI (smile ^. tenor) alpha beta rho m sigma

