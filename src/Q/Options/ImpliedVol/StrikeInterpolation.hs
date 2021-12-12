module Q.Options.ImpliedVol.StrikeInterpolation where


import qualified Data.SortedList as SortedList
import qualified Q.Interpolation as Interpolation
import           Q.Options.ImpliedVol.StrikeSpace
import           Q.Types

data InterpolationMethod = Linear | Cubic | Akima
data ExtrapolationMethod = Constant

data StrikeInterpolation x = StrikeInterpolation InterpolationMethod (SortedList.SortedList (x, Vol))


mkInterpolator :: (StrikeSpace x) => InterpolationMethod -> SortedList.SortedList (x, Vol) -> Interpolation.Interpolation x Vol
mkInterpolator  Linear kvs = Interpolation.linearInterpolator strikes vols
  where (strikes ,vols) = (unzip . SortedList.fromSortedList) kvs
mkInterpolator Cubic kvs = Interpolation.cubicSplineInterpolator strikes vols
  where (strikes ,vols) = (unzip . SortedList.fromSortedList) kvs
mkInterpolator Akima kvs = Interpolation.akimaSplineInterpolator strikes vols
  where (strikes ,vols) = (unzip . SortedList.fromSortedList) kvs

