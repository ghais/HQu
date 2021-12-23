{-|
Module      : Q.Option.ImpliedVol.StrikeSpace
Description : implied volatility surface strike space representation and conversion.
Copyright   : (c) 0x47@0x49.dev, 2021
License     : MIT
Maintainer  : 0x47@0x49.dev
Stability   : experimental
Portability : portable

The strike space represnets the coordinate the vol surface is in such as sticky-strike or relative-strike..etc.
This module defines what a strike space is and conversions between the different strike types.
-}
module Q.Options.ImpliedVol.StrikeSpace
  (
    StrikeSpace(..)
  ,remap
  ) where

import           Data.Coerce (Coercible)
import           Q.Types (AbsRelStrike (..), Forward (..), LogRelStrike (..), Spot, Strike (..),
                          YearFrac)
-- | Define a strike space.
class (Ord x, Coercible x Double) => StrikeSpace x where
  -- | Return the at them money forward for a given strike space.
  atmfStrike :: YearFrac -> Spot -> Forward -> x

  -- | Translate a strike in the strike-space to a cash strike
  strikeSpaceToCash :: x -> YearFrac -> Spot -> Forward -> Strike

  -- | translate a cash strike to the strike space.
  cashToStrikeSpace :: Strike -> YearFrac -> Spot -> Forward -> x

  -- | Bump a strike up.
  bumpUp   :: x -> Double -> x

  -- | Bump a strike down.
  bumpDown :: x -> Double -> x
  bumpDown x eps = bumpUp x (negate eps)

instance StrikeSpace Strike where
  atmfStrike _ _ (Forward f)          = Strike f
  strikeSpaceToCash x _ _ _ = x
  cashToStrikeSpace k _ _ _ = k
  bumpUp (Strike k) eps  = Strike $ k + eps


instance StrikeSpace AbsRelStrike where
  atmfStrike  _ _ _                            = 0
  strikeSpaceToCash (AbsRel x) _ _ (Forward f) = Strike $ x + f
  cashToStrikeSpace (Strike k) _ _ (Forward f) = AbsRel $ k - f
  bumpUp (AbsRel k) eps  = AbsRel $ k + eps

instance StrikeSpace LogRelStrike where
  atmfStrike  _ _ _                            = 0
  strikeSpaceToCash (LogRel x) _ _ (Forward f) = Strike $ f * exp x
  cashToStrikeSpace (Strike k) _ _ (Forward f) = LogRel $ log $ k/f
  bumpUp (LogRel k) eps  = LogRel $ k + eps


-- | Remap one strike space to another by going through the cash strike.
remap :: (StrikeSpace x, StrikeSpace y) => x -> YearFrac -> Spot -> Forward ->  y
remap x t s f = let k = strikeSpaceToCash x t s f
                in cashToStrikeSpace k t s f
