module Q.Options.ImpliedVol.TimeInterpolation
  (
    TimeInterpolation(..)
  , TimeExtrapolation(..)
  ) where

-- | Method of interpolation between tenors
data TimeInterpolation = LinearInVol      -- ^ Linear interpolate in volatility space.
                       | LinearInTotalVar -- ^ Linear interpolate in total variance space.

-- | Method of extrapolating tenors before the first and after the last tenor.
data TimeExtrapolation = TerminalMoneyness -- ^ Constant volatility at the same moneyness.


