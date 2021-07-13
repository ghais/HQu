{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Q.Options.ImpliedVol.SVI
  (
    -- * SVI constructors
    SVI(..)

    -- * 'RSVI' params
  , Alpha(..)
  , Beta(..)
  , Rho(..)
  , M(..)
  , Sigma(..)

    -- * 'JWSVI' params
  , V(..)
  , Phi(..)
  , P(..)
  , C(..)
  , VTil(..)

    -- * Convert between 'RSVI' and 'JWSVI' and vice versa
  , jwSVIToR
  , rSVIToJW
    -- * Helper functions.
  , isValidSVI
  , rSVIParams
  , jwSVIParams

  ) where
import           GHC.Generics (Generic)

import           Q.Options.ImpliedVol.TimeSlice
import           Q.Types (TotalVar (TotalVar), YearFrac (..), LogRelStrike(..))


-- | Stochastic volatility inspired parameterization of the vol surface.
data SVI = RSVI     -- ^ The original raw SVI representation from Gatheral
           YearFrac -- ^ TTM
           Alpha    -- ^ Corresponds to a vertical translation of the smile.
           Beta     -- ^ Slope of call and put wings.
           Rho      -- ^ A counter clock wise rotation of the smile.
           M        -- ^ translate the smile to the right
           Sigma    -- ^ ATM curvature of the smile.
         | JWSVI    -- ^ The SVI-JW parameterization from Gatheral
           YearFrac -- ^ TTM
           V        -- ^ ATM variance
           Phi      -- ^ ATM skew
           P        -- ^ Slope of the left (put) wing.
           C        -- ^ Slope of the right (call) wing
           VTil   -- ^ The minimum implied variance.
           deriving stock Show


newtype Alpha  = Alpha  Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Beta   = Beta   Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Rho    = Rho    Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype M      = M      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Sigma  = Sigma  Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)

newtype V      = V      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Phi    = Phi    Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype P      = P      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype C      = C      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype VTil   = VTil   Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)

instance TimeSlice SVI LogRelStrike  where
  totalVar (RSVI _ (Alpha 𝜶) (Beta 𝜷) (Rho 𝛒) (M 𝐦) (Sigma 𝛔)) (LogRel 𝐤) =
    TotalVar $ 𝜶 + 𝜷 * (𝛒 * (𝐤 - 𝐦) + sqrt ((𝐤 - 𝐦) ** 2 + 𝛔 * 𝛔))

  totalVar (JWSVI (YearFrac ttm) (V v) (Phi phi) (P p) (C c) (VTil vtil)) k =
    totalVar (RSVI (YearFrac ttm) (Alpha a) (Beta b) (Rho rho) (M m) (Sigma sigma)) k
    where wt    = v * ttm
          vt = sqrt wt
          b = 0.5 *vt * (p + c)
          rho = 1- p * vt / b
          beta = rho - 2.0 * phi * vt / b
          alpha = signum beta* sqrt(1.0/beta**2 - 1.0)
          m = (v - vtil)*ttm / (b * (-rho + signum alpha* sqrt(1.0 + alpha**2) - alpha* sqrt(1-rho**2)))
          sigma = alpha * m
          a = vtil*ttm - b*sigma* sqrt(1.0 - rho**2)

  dW svi k = dW (sviTV svi) k

  d2W svi k = d2W (sviTV svi) k

  impliedDensity svi k = impliedDensity (sviTV svi) k


-- | Convert Raw-SVI ('RSVI') parameters to JW-SVI ('JWSVI')
rSVIToJW :: YearFrac -> Alpha -> Beta -> Rho -> M -> Sigma -> SVI
rSVIToJW (YearFrac ttm) (Alpha a) (Beta b) (Rho rho) (M m) (Sigma sigma) =
  JWSVI (YearFrac ttm) (V v_t) (Phi phi_t) (P p_t) (C c_t) (VTil vtil_t)
  where  m2s2 = sqrt(m ** 2 + sigma ** 2)
         v_t = (a + b * (-rho * m + m2s2)) / ttm
         w_t = v_t * ttm
         vol = sqrt(w_t)
         c2 = if m2s2 > 0 then m / m2s2  else 0
         phi_t = (-c2 + rho) * (0.5 * b) / vol
         p_t = b * (1 - rho) / vol
         c_t = b * (1 + rho) / vol
         vtil_t = (a + b * sigma * sqrt(1 - rho ** 2)) / ttm

-- | Convert JW-SVI ('JWSVI') parameters to Raw-SVI ('RSVI')
jwSVIToR :: YearFrac -> V -> Phi -> P -> C -> VTil -> SVI
jwSVIToR (YearFrac ttm) (V v) (Phi phi) (P p) (C c) (VTil vtil) =
  RSVI (YearFrac ttm) (Alpha a) (Beta b) (Rho rho) (M m) (Sigma sigma)
    where wt    = v * ttm
          vt = sqrt wt
          b = 0.5 *vt * (p + c)
          rho = 1- p * vt / b
          beta = rho - 2.0 * phi * vt / b
          alpha = signum beta* sqrt(1.0/beta**2 - 1.0)
          m = (v - vtil)*ttm / (b * (-rho + signum alpha* sqrt(1.0 + alpha**2) - alpha* sqrt(1-rho**2)))
          sigma = alpha * m
          a = vtil*ttm - b*sigma* sqrt(1.0 - rho**2)


-- | Check if an SVI parameterization is valid
isValidSVI :: SVI -> Bool
isValidSVI (RSVI _ (Alpha 𝜶) (Beta 𝜷) (Rho 𝛒) (M _) (Sigma 𝛔)) =
    𝜷 >= 0
  && abs 𝛒 < 1
  && 𝛔 > 0
  && 𝜶 + 𝜷 * 𝛔 * sqrt (1 -𝛒*𝛒) >= 0
isValidSVI (JWSVI ttm v phi p c vtil) = isValidSVI (jwSVIToR ttm v phi p c vtil)

sviTV :: SVI -> LogRelStrike -> TotalVar
sviTV = totalVar

-- | Extract the JW-SVI parameters
jwSVIParams :: SVI -> (YearFrac, V, Phi, P, C, VTil)
jwSVIParams (JWSVI ttm v phi p c vtil) = (ttm, v, phi, p, c, vtil)
jwSVIParams (RSVI ttm alpha beta rho m sigma) = jwSVIParams (rSVIToJW ttm alpha beta rho m sigma)

-- | Extract the R-SVI parameters.
rSVIParams :: SVI -> (YearFrac, Alpha, Beta, Rho, M, Sigma)
rSVIParams (RSVI ttm alpha beta rho m sigma) = (ttm, alpha, beta, rho, m, sigma)
rSVIParams (JWSVI ttm v phi p c vtil) = rSVIParams (jwSVIToR ttm v phi p c vtil)
