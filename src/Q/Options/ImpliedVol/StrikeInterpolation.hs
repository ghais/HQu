module Q.Options.ImpliedVol.StrikeInterpolation where

import qualified Q.Interpolation as Interpolation
import           Q.Types
import qualified Data.SortedList as SortedList

data InterpolationMethod = Linear | Cubic | Akima
data ExtrapolationMethod = Constant
                         | ConstantGradient
                         | ConstantCurvature

data StrikeInterpolation = StrikeInterpolation InterpolationMethod ExtrapolationMethod (SortedList.SortedList (Strike, Vol))


mkInterpolator :: StrikeInterpolation -> Interpolation.Interpolation Strike Vol
mkInterpolator (StrikeInterpolation Linear _ kvs) = Interpolation.linearInterpolator strikes vols
  where (strikes ,vols) = (unzip . SortedList.fromSortedList) kvs
mkInterpolator (StrikeInterpolation Cubic _ kvs) = Interpolation.cubicSplineInterpolator strikes vols
  where (strikes ,vols) = (unzip . SortedList.fromSortedList) kvs
mkInterpolator (StrikeInterpolation Akima _ kvs) = Interpolation.akimaSplineInterpolator strikes vols
  where (strikes ,vols) = (unzip . SortedList.fromSortedList) kvs



instance Interpolation.Interpolator StrikeInterpolation Strike Vol where
  interpolate method  = Interpolation.interpolate ( mkInterpolator method)
  derivative method  = Interpolation.derivative (mkInterpolator method)
  secondDerivative method = Interpolation.secondDerivative (mkInterpolator method)
  xMin method  = Interpolation.xMin (mkInterpolator method)
  xMax method = Interpolation.xMax (mkInterpolator method)


  
