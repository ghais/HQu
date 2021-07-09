{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Q.Options.ImpliedVol.SVI where
import           Q.Types      (Forward (..), Strike (..), Vol (..),
                               YearFrac (..))
import           Q.Options.ImpliedVol.TimeSlice
import           GHC.Generics (Generic)
import Q.Greeks (Bump, Bumpable(..))

newtype Alpha  = Alpha  Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype Beta   = Beta   Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype Rho    = Rho    Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype M      = M      Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)
newtype Sigma  = Sigma  Double deriving (Generic, Eq, Show, Ord, Num, Fractional, Floating)

-- | Stochastic volatility inspired parameterization of the vol surface.
data SVI = RSVI    -- ^ The original raw SVI representation from Gatheral
           Alpha   -- ^ Corresponds to a vertical translation of the smile.
           Beta    -- ^ Slope of call and put wings.
           Rho     -- ^ A counter clock wise rotation of the smile.
           M       -- ^ translate the smile to the right
           Sigma   -- ^ ATM curviture of the smile.

instance TimeSlice SVI LogRelStrike  where
  totalVar (RSVI (Alpha 𝜶) (Beta 𝜷) (Rho 𝛒) (M 𝐦) (Sigma 𝛔)) (LogRel 𝐤) =
    TotalVar $ 𝜶 + 𝜷 * (𝛒 * (𝐤 - 𝐦) + sqrt ((𝐤 - 𝐦) ** 2 + 𝛔 * 𝛔))


isValidSVI (RSVI (Alpha 𝜶) (Beta 𝜷) (Rho 𝛒) (M 𝐦) (Sigma 𝛔)) =
    𝜷 >= 0
  && abs 𝛒 < 1
  && 𝛔 > 0
  && 𝜶 + 𝜷 * 𝛔 * sqrt (1 -𝛒*𝛒) >= 0
