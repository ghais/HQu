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

import qualified Data.Vector.Algorithms.Merge  as V (sort)

import           Data.Vector.Storable  (Storable)
import qualified Data.Vector.Storable as V (Vector (..), fromList, length, head, last, modify)
import           Q.Types

newtype SortedVector a = SortedVector (V.Vector a)

fromList as = SortedVector (V.modify V.sort $ V.fromList as)
fromVector v = SortedVector (V.modify V.sort v)
fromSortedList xs = SortedVector $ V.fromList xs


minElement (SortedVector v) = V.head v
maxElement (SortedVector v) = V.last v
