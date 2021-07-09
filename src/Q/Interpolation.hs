{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Q.Interpolation where
import qualified Q.SortedVector as SV
import Numeric.GSL.Interpolation
import qualified Numeric.LinearAlgebra as V (Vector, fromList)
import Foreign (Storable)
import Data.List
class (Ord k, Storable k, Storable v) => Interpolator a k v where
  interpolate :: a -> [(k, v)] -> k -> v

class (Ord k, Storable k, Storable v) => InterpolatorV a k v where
  interpolateV :: a -> SV.SortedVector k -> V.Vector v -> k -> v

instance (Ord k, Storable k, Storable v, InterpolatorV a k v) => Interpolator a k v where
  interpolate a pts = interpolateV a xs' ys' where
    (xs, ys) = (unzip . sortOn fst) pts
    xs'      = SV.fromSortedList xs
    ys'      = V.fromList ys
