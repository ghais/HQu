module Q.Options.ImpliedVol.StrikeSpace where

import Q.Types
import Data.Coerce

class (Ord k, Coercible k Double) => StrikeSpace k where
  atmfStrike :: YearFrac -> Spot -> Forward -> k
  strikeSpaceToCash :: k -> YearFrac -> Spot -> Forward -> Strike
  cashToStrikeSpace :: Strike -> YearFrac -> Spot -> Forward ->  k
  bumpUp   :: k -> Double -> k
  bumpDown :: k -> Double -> k
  bumpDown k eps = bumpUp k (negate eps)

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
