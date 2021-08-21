module Q.Options.Black76
  (
    Black76(..)
  , atmf
  , euOption
  , eucall
  , euput
  , dPlus
  , dMinus
  , B76Error
  , B76Monad
  )
  where


import           Numeric.GSL (derivCentral)
import           Q.Options (DF, Delta (Delta), Forward (..), Gamma (Gamma), OptionType (..),
                            Premium (Premium), Strike (..), TimeScaleable (scale), TotalVar (..),
                            Valuation (Valuation, vPremium), Vega (Vega), Vol (..),
                            YearFrac (YearFrac), discount)
import           Q.Options.ImpliedVol.TimeSlice (TimeSlice (..))
import           Statistics.Distribution (cumulative, density)
import           Statistics.Distribution.Normal (standard)
import Q.Types (LogRelStrike)
import Control.Monad.Except


data Black76 = Black76 {
    b76F   :: Forward
  , b76DF  :: DF
  , b76T   :: YearFrac
  , b76Vol :: Vol
}

data B76Error = NegativeVol Vol
              | InvalidParameter String
              | NoSolution String
              | NoImpliedVol String
              deriving stock (Show, Eq)

type B76Monad = Except B76Error

-- | At the money forward strike.
atmf :: Black76 -> Strike
atmf Black76{..} = Strike f
  where (Forward f) = b76F

-- | European option valuation with black 76
euOption :: Black76 -> OptionType -> Strike -> Valuation
euOption Black76{..} cp k = Valuation premium delta vega gamma where
  (Forward f) = b76F
  n           = cumulative standard
  (Vol sigmaSqt) = scale b76T b76Vol
  d1          = dPlus  b76F b76Vol k b76T
  d2          = dMinus b76F b76Vol k b76T
  nd1         = n d1
  nd2         = n d2
  callDelta   = b76DF `discount` nd1
  putDelta    = b76DF `discount` (- (n (-d1)))
  vega        = Vega  $ b76DF `discount` density standard d1 * f * sigmaSqt
  gamma       = Gamma $ b76DF `discount` density standard d1 / (f * sigmaSqt)
  premium  = Premium $ case cp of
    Call -> b76DF `discount` (f * nd1 - nd2 * k')
    Put  -> b76DF `discount` (n (-d2) * k' - n (-d1) * f)
    where (Strike k') = k
  delta | cp == Call = Delta callDelta
        | otherwise = Delta putDelta

-- | see 'euOption'
euput :: Black76 -> Strike -> Valuation
euput b76 =  euOption b76 Put

-- | see 'euOption'
eucall :: Black76 -> Strike -> Valuation
eucall b76 = euOption b76 Call

dPlus :: Forward -> Vol -> Strike -> YearFrac -> Double
dPlus (Forward f) (Vol sigma) (Strike k) (YearFrac t) =
  recip (sigma * sqrt t) * (log (f/k) + 0.5 * sigma * sigma * t)

dMinus :: Forward -> Vol -> Strike -> YearFrac -> Double
dMinus (Forward f) (Vol sigma) (Strike k) (YearFrac t) =
  recip (sigma * sqrt t) * (log (f/k) - 0.5 * sigma * sigma * t)


instance TimeSlice Black76 Strike where
  totalVar Black76{..} _ = TotalVar $ vol * vol * t
    where (Vol vol) = b76Vol
          (YearFrac t) = b76T
  dW _ _  = 0
  d2W _ _ = 0

  impliedDensity b76 (Strike k) = let
    dk k' = fst (derivCentral 1e-4 (\k2 -> let (Premium v) = vPremium (eucall b76 (Strike k2)) in v) k')
    in fst (derivCentral 1e-4 dk k)

instance TimeSlice Black76 LogRelStrike  where
  totalVar Black76{..} _ = TotalVar $ vol * vol * t
    where (Vol vol) = b76Vol
          (YearFrac t) = b76T
  dW _ _  = 0
  d2W _ _ = 0

  impliedDensity bs k = impliedDensity f k where
    f :: LogRelStrike  -> TotalVar
    f = totalVar bs
