{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Q.Greeks
  (
    module Q.Types
  , module Q.Options
  , Bump (..)
  , DiffMethod(..)
  , Bumpable(..)
  , firstOrder
  ) where

import Q.Types
import Q.Options
import Data.Coerce

-- | A relative or an absolute bump. Used with numerical Greeks.
data Bump = Abs Double
          | Rel Double

data DiffMethod = ForwardDiff
                | BackwardDiff
                | CenteralDiff

class Bumpable a where
  bumpUp   :: a -> Bump -> a
  bumpDown :: a -> Bump -> a
  stepSize :: a -> Bump -> Double

-- | Things we can bump to calculate Greeks such as 'Spot', 'Rate'..etc'
instance (Coercible a Double) => Bumpable a where
  bumpUp a (Abs bump) = coerce $ coerce a + bump
  bumpUp a (Rel bump) = coerce $ coerce a * (1 + bump)

  bumpDown a (Abs bump) = coerce $ coerce a - bump
  bumpDown a (Rel bump) = coerce $ coerce a * (1 - bump)

  stepSize _ (Abs bump)  = bump
  stepSize s (Rel bump) = coerce s * bump



firstOrder :: (Bumpable a) => DiffMethod -> Bump -> (a -> Double) -> a -> Double
firstOrder ForwardDiff b f a =  df / dx
  where df = f a' - f a
        a' = bumpUp a b
        dx = stepSize a b :: Double

firstOrder BackwardDiff d f a = df / dx
   where df = f a - f a'
         a' = bumpDown a d
         dx = negate (stepSize a d )

firstOrder CenteralDiff b f a = df / dx
   where df = f u - f d
         u = bumpUp a b
         d = bumpDown a b
         dx = 2 * stepSize a b

