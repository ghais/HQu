module Q.Payoff where

import Q.Types
import Q.Time


class Payoff a where
  payoff :: (Obs1 b) => a      -- ^ The instrument.
                    -> b      -- ^ The observable at the payoff time.
                    -> Cash -- ^ Payoff amount.

-- | Path independent payoffs based on a fixed strike.
data StrikedPayoff =
  -- | Vanilla option payoff \(max (s - k, 0)\)
  --   for call and \(max (k - s, 0)\) for put
  PlainVanillaPayoff
      OptionType         -- ^ Call/Put indicator
      Strike             -- ^ Strike \(k\)
  -- | Payoff with strike expressed as percentage
  | PercentagePayoff
      OptionType         -- ^ Call/Put indicator
      Strike             -- ^ Strike in percentage.
  -- | Binary asset or nothing payoff.
  | AssetOrNothingPayoff
      OptionType         -- ^ Call/Put indicator
      Strike             -- ^ Strike \(k\)
  -- | Binary cash or nothing payoff.
  | CashOrNothingPayoff
      OptionType         -- ^ Call/Put indicator
      Strike             -- ^ Strike \(k\)
      Cash               -- ^ Cash amount.
 

instance Payoff StrikedPayoff where
  payoff (PlainVanillaPayoff cp (Strike k)) obs = Cash $ max ((cpi cp) * (s - k)) 0 where
    s = get1 obs

  payoff (PercentagePayoff cp (Strike k)) _ =  Cash $ max ((cpi cp) * (1 - k)) 0

  payoff (AssetOrNothingPayoff cp (Strike k)) obs
    | (cpi cp) * (s - k) > 0 = Cash $ s
    | otherwise              = Cash $ 0
    where s = get1 obs
  payoff (CashOrNothingPayoff cp (Strike k) (Cash amount)) obs
    | (cpi cp) * (s - k) > 0 = Cash $ amount
    | otherwise = 0
    where s = get1 obs
