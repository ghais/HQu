{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Q.Options.ImpliedVol.SVI
  (
    -- * SVI constructors
    SVI(..)

  -- * Parameters
  , Alpha(..)
  , Beta(..)
  , Rho(..)
  , M(..)
  , Sigma(..)
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
  -- * Analytical fitting for SVI3P
  , analyticalSVI3P

  ,SSVI(..)) where


import           Q.Options.ImpliedVol.TimeSlice
import           Q.Types
import           Q.Options.ImpliedVol.Surface

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
           VTil     -- ^ The minimum implied variance.
         | SVI3P    -- ^ A specialization of the Global Surface SVI where we assume
                    --   no restriction in time between alpha and raw.
           YearFrac
           Alpha
           Rho
           Sigma
         deriving Show

data SSVI = SSVI                   -- ^ Surface SVI with Heston-like parameterization
            (YearFrac -> TotalVar) -- ^ ATM total variance
            Rho                    -- ^ Rho
            Lambda                 -- ^ Lambda



instance VolSurface SSVI LogRelStrike where
    surfaceTotalVarKT (SSVI atmTotalVar (Rho rho) (Lambda lambda)) (LogRel k) t =
      let (TotalVar theta) = atmTotalVar t
          psi = (1/(theta * lambda)) * (1 - (1 - exp (-lambda * theta))/lambda*theta)
      in TotalVar $ theta/2 * (1 + rho * psi * k + sqrt ((psi*k + rho)**2 + (1 - rho**2)))


instance TimeSlice SVI LogRelStrike  where
  totalVar (RSVI _ (Alpha 𝜶) (Beta 𝜷) (Rho 𝛒) (M 𝐦) (Sigma 𝛔)) (LogRel 𝐤) =
    TotalVar $ 𝜶 + 𝜷 * (𝛒 * (𝐤 - 𝐦) + sqrt ((𝐤 - 𝐦) ** 2 + 𝛔 * 𝛔))

  totalVar (JWSVI  ttm v phi p c vtil) k =
    totalVar (jwSVIToR ttm v phi p c vtil) k

  totalVar (SVI3P (YearFrac ttm) (Alpha alpha) (Rho rho) (Sigma sigma)) (LogRel k) =
    TotalVar $ max 0.0 (0.5 * totVar * (1 + rho * phi * k + sqrt((phi * k + rho)** 2 + 1 - rho**2)))
      where totVar = ttm * sigma * sigma
            phi    = alpha / sigma / sqrt ttm

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
         vol = sqrt w_t
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
jwSVIParams _ = error "Cannot do this conversion"

-- | Extract the R-SVI parameters.
rSVIParams :: SVI -> (YearFrac, Alpha, Beta, Rho, M, Sigma)
rSVIParams (RSVI ttm alpha beta rho m sigma) = (ttm, alpha, beta, rho, m, sigma)
rSVIParams (JWSVI ttm v phi p c vtil) = rSVIParams (jwSVIToR ttm v phi p c vtil)
rSVIParams _ = error "Cannot do this conversion"


analyticalSVI3P :: YearFrac -> Vol -> (LogRelStrike, Vol) -> (LogRelStrike, Vol) -> SVI
analyticalSVI3P (YearFrac t) (Vol atmVol) (LogRel k1, Vol vol1) (LogRel k2, Vol vol2) =
  let atmVolSqrtT = atmVol * sqrt t
      y1          = k1/atmVolSqrtT
      v1          = (vol1 / atmVol)**2
      y2          = k2/atmVolSqrtT
      v2          = (vol2 / atmVol)**2
      ra          = negate (v2 * (1 - v2) - v1 * (1 - v1)) / (v2 * y2 - v1 * y1)
      aa          = ra * ra - 4 * ra * v2 / y2 - 4 * v2 * (1 - v2) / y2**2

  in if aa < 1e-10 then
       SVI3P (YearFrac t) (Alpha 0) (Rho 0) (Sigma atmVol)
    else if (aa > 0) && (aa > ra * ra) then
       let alpha = sqrt aa
           rho   = ra / alpha
       in SVI3P (YearFrac t) (Alpha alpha) (Rho rho) (Sigma atmVol)
    else
       let alpha = if ra >= 0.0 then ra else -ra
           rho   = if ra >= 0.0 then 1.0 else -1.0
       in SVI3P (YearFrac t) (Alpha alpha) (Rho rho) (Sigma atmVol)
