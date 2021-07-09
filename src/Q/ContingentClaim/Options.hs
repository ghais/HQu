module Q.ContingentClaim.Options where

import           Data.Time
import           Q.ContingentClaim
import           Q.Types

vanillaPayout :: OptionType  -- ^ Put or call
              -> Double      -- ^ strike
              -> Double      -- ^ Observable level
              -> Double      -- ^ Payout
vanillaPayout Call k s = max (s - k) 0
vanillaPayout Put  k s = max (k - s) 0


spreadPayout :: OptionType -- ^ Put or call
             -> Double     -- ^ Low strike
             -> Double     -- ^ High strike
             -> Double     -- ^ Observable level
             -> Double     -- ^ Payout

straddlePayout :: Double -- ^ Strike
               -> Double -- ^ Observable
               -> Double -- ^ Payout
straddlePayout k s = (vanillaPayout Call k s) + (vanillaPayout Put k s)

spreadPayout Call lowStrike highStrike s = (vanillaPayout Call lowStrike s) - (vanillaPayout Call highStrike s)
spreadPayout Put lowStrike highStrike s = (vanillaPayout Put highStrike s) - (vanillaPayout Put lowStrike s)

vanillaOption :: OptionType -- ^ Option type
  -> Double                 -- ^ Strike
  -> LocalTime              -- ^ Expiry
  -> ContingentClaim Double -- ^ Contingent claim
vanillaOption cp k t = pay t $ do
  s <- monitor t
  return $ CashFlow t $ vanillaPayout cp k s

callOption = vanillaOption Call
putOption = vanillaOption Put

-- | A call spread is a portfolio: \(C(K1, T) - C(K2 T) \) s.t. \( K1 < K2 \)
callSpread k1 k2 t = (vanillaOption Call k1 t) <> (short $ vanillaOption Call k2 t)

-- | A put spread is a portfolio: \(P(K2, T) - P(K1 T) \) s.t. \( K1 < K2 \)
putSpread k1 k2 t = (vanillaOption Put k2 t) <> (short $ vanillaOption Put k1 t)

-- | A straddle is a a portfolio :\(C(K, T) + Put(K, T)\)
straddle :: Double -> LocalTime -> ContingentClaim Double
straddle strike t = vanillaOption Put strike t <> vanillaOption Call strike t
