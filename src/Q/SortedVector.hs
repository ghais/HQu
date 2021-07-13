{-# LANGUAGE FlexibleContexts #-}
module Q.SortedVector
  (
    fromList
  , fromVector
  , fromSortedList
  , SortedVector(..)
  , minElement
  , maxElement
  ) where

import qualified Data.Vector.Algorithms.Merge  as Merge (sort)

import qualified Data.Vector.Storable as V (Storable, Vector, fromList, head, last, modify)


-- | A sorted vector.
newtype SortedVector a = SortedVector (V.Vector a)

-- | construct a 'SortedVector' from a list
fromList :: (V.Storable a, Ord a) => [a] -> SortedVector a
fromList as = SortedVector (V.modify Merge.sort $ V.fromList as)

-- | construct a sorted vector from 
fromVector :: (V.Storable a, Ord a) => V.Vector a -> SortedVector a
fromVector v = SortedVector (V.modify Merge.sort v)

fromSortedList :: V.Storable a => [a] -> SortedVector a
fromSortedList xs = SortedVector $ V.fromList xs


minElement :: V.Storable a => SortedVector a -> a
minElement (SortedVector v) = V.head v
maxElement :: V.Storable a => SortedVector a -> a
maxElement (SortedVector v) = V.last v
