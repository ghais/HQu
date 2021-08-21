{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RoleAnnotations #-}

module Q.SortedVector
  (
   SortedVector(..)
  , fromList
  , fromSortedList
  , fromVector
  , toList
  , minElement
  , maxElement  
  , Q.SortedVector.null
  , Q.SortedVector.length
  , Q.SortedVector.zip
  ) where

import Prelude hiding (head, null, length, zip)
import qualified Data.Vector.Algorithms.Merge  as Merge (sort)
import qualified Data.Vector as V (Vector, fromList, head, last, modify, null, toList, length, zip)

import Data.Data (Data)
import qualified GHC.Exts as Exts



-- | A sorted vector.
newtype SortedVector a = SortedVector (V.Vector a) deriving stock (Eq, Data, Ord, Read, Show)
                                                   deriving newtype (Semigroup, Monoid)

type role SortedVector nominal

instance (Ord a) => Exts.IsList (SortedVector a) where
  type Item (SortedVector a) = a
  fromList = fromList
  fromListN _ = fromList
  toList = toList

-- | construct a 'SortedVector' from a list
fromList :: (Ord a) => [a] -> SortedVector a
fromList as = SortedVector (V.modify Merge.sort $ V.fromList as)

fromSortedList :: [a] -> SortedVector a
fromSortedList xs = SortedVector $ V.fromList xs

toList :: SortedVector a -> [a]
toList (SortedVector v) = V.toList v

-- | construct a sorted vector from
fromVector :: (Ord a) => V.Vector a -> SortedVector a
fromVector v = SortedVector (V.modify Merge.sort v)


minElement :: SortedVector a -> a
minElement (SortedVector v) = V.head v
maxElement :: SortedVector a -> a
maxElement (SortedVector v) = V.last v


null :: SortedVector a -> Bool
null (SortedVector v) = V.null v

length :: SortedVector a -> Int
length (SortedVector v) = V.length v

zip :: SortedVector a -> V.Vector b -> SortedVector (a,b)
zip (SortedVector xs) ys = SortedVector (V.zip xs ys)
