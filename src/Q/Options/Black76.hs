module Q.Options.Black76
  (
    Black76(..)
  , atmf
  , euOption
  , eucall
  , theta1D
  , euput
  , dPlus
  , dMinus
  , B76Error
  , B76Monad
  )
  where


import           Control.Monad.Except
import           Data.Coerce
import           Numeric.GSL (derivCentral)
import           Q.Options
import           Q.Options.ImpliedVol.TimeSlice (TimeSlice (..), ImpliedDensity(..))
import Q.Types
    ( discount,
      rateFromDiscount,
      DF,
      Delta(Delta),
      Forward(..),
      Gamma(Gamma),
      LogRelStrike,
      OptionType(..),
      Premium(Premium),
      Rate(..),
      Rho(Rho),
      Strike(..),
      Theta(Theta),
      TimeScaleable(scale),
      TotalVar(..),
      Vega(Vega),
      Vol(..),
      YearFrac(YearFrac) )
import           Statistics.Distribution (cumulative, density)
import           Statistics.Distribution.Normal (standard)



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
euOption Black76{..} cp k = Valuation premium delta vega gamma theta rho where
  (Forward f)     = b76F
  (YearFrac t)    = b76T
  (Rate r)        = rateFromDiscount b76T b76DF
  (Strike strike) = k
  n               = cumulative standard
  (Vol sigmaSqt)  = scale b76T b76Vol
  (Vol sigma)     = b76Vol
  d1              = dPlus  b76F b76Vol k b76T
  d2              = dMinus b76F b76Vol k b76T
  nd1             = n d1
  nd2             = n d2
  nd1'            = n (-d1)
  nd2'            = n (-d2)
  callDelta       = Delta $ b76DF `discount` nd1
  putDelta        = Delta $ b76DF `discount` (- (n (-d1)))
  vega            = Vega  $ b76DF `discount` density standard d1 * f * sigmaSqt
  gamma           = Gamma $ b76DF `discount` density standard d1 / (f * sigmaSqt)
  callTheta       = Theta $ b76DF `discount` ((negate $ f * nd1 * sigma / (2 * sqrt t)) + r * f * nd1  - r * strike * nd2)
  putTheta        = Theta $ b76DF `discount` ((negate $ f * nd1 * sigma / (2 * sqrt t)) - r * f * nd1' + r * strike * nd2')
  rho             = Rho   $ -t * coerce premium
  premium         = Premium $ case cp of
    Call -> b76DF `discount` (f * nd1 - nd2 * k')
    Put  -> b76DF `discount` (nd2' * k' - nd1' * f)
    where (Strike k') = k
  delta | cp == Call = callDelta
        | otherwise = putDelta
  theta | cp == Call = callTheta
        | otherwise = putTheta

-- | see 'euOption'
euput :: Black76 -> Strike -> Valuation
euput b76 =  euOption b76 Put

-- | see 'euOption'
eucall :: Black76 -> Strike -> Valuation
eucall b76 = euOption b76 Call

theta1D :: Black76 -> OptionType -> Strike -> Theta1D
theta1D b cp k = coerce $ vPremium (euOption (decayOneDay b) cp k) - vPremium (euOption b cp k)

-- | see 'euOption'

decayOneDay :: Black76 -> Black76
decayOneDay (Black76 f df t sigma) = if t < oneDay then
                                        Black76 f df (YearFrac 0.0001) sigma
                                      else
                                        Black76 f df (t - oneDay) sigma
  where oneDay = 1/365


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

instance ImpliedDensity Black76 Strike where 
  dW _ _  = 0
  d2W _ _ = 0

  impliedDensity b76 (Strike k) = let
    dk k' = fst (derivCentral 1e-4 (\k2 -> let (Premium v) = vPremium (eucall b76 (Strike k2)) in v) k')
    in fst (derivCentral 1e-4 dk k)

instance TimeSlice Black76 LogRelStrike  where
  totalVar Black76{..} _ = TotalVar $ vol * vol * t
    where (Vol vol) = b76Vol
          (YearFrac t) = b76T

instance ImpliedDensity Black76 LogRelStrike  where
  dW _ _  = 0
  d2W _ _ = 0

  impliedDensity bs k = impliedDensity f k where
    f :: LogRelStrike  -> TotalVar
    f = totalVar bs



