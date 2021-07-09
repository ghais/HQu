{-# LANGUAGE RecordWildCards #-}
module Q.Options.Black76
  (
    module Q.Options
  , Black76(..)
  , atmf
  , euOption
  , eucall
  , euput
  )
  where

import           Q.Options
import           Q.Types
import           Statistics.Distribution        (cumulative, density)
import           Statistics.Distribution.Normal (standard)

data Black76 = Black76 {
    b76F   :: Forward
  , b76DF  :: DF
  , b76T   :: YearFrac
  , b76Vol :: Vol
}

-- | At the money forward strike.
atmf :: Black76 -> Strike
atmf Black76{..} = Strike f
  where (Forward f) = b76F

-- | European option valuation with black 76
euOption :: Black76 -> OptionType -> Strike -> Valuation
euOption b76@Black76{..} cp k = Valuation premium delta vega gamma where
  (Forward f) = b76F
  n           = cumulative standard
  (Vol sigmaSqt) = scale b76T b76Vol
  d1          = (dPlus  b76F b76Vol k b76T)
  d2          = (dMinus b76F b76Vol k b76T)
  nd1         = n d1
  nd2         = n d2
  callDelta   = b76DF `discount` nd1
  putDelta    = b76DF `discount` (- (n (-d1)))
  vega        = Vega  $ b76DF `discount` (density standard d1 ) * f * sigmaSqt
  gamma       = Gamma $ b76DF `discount` (density standard d1) / (f * sigmaSqt)
  premium  = Premium $ case cp of
    Call -> b76DF `discount` (f * nd1 - nd2 * k')
    Put  -> b76DF `discount` (n (-d2) * k' - n (-d1) * f)
    where (Strike k') = k
  delta | cp == Call = Delta $ callDelta
        | cp == Put  = Delta $ putDelta

-- | see 'euOption'
euput b76 =  euOption b76 Put

-- | see 'euOption'
eucall b76 = euOption b76 Call

dPlus (Forward f) (Vol sigma) (Strike k) (YearFrac t) =
  recip (sigma * sqrt t) * (log (f/k) + (0.5 * sigma * sigma) * t)
dMinus (Forward f) (Vol sigma) (Strike k) (YearFrac t) =
  recip (sigma * sqrt t) * (log (f/k) - (0.5 * sigma * sigma) * t)
