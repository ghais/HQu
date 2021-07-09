module Q.Currencies.Asia
  (
    module Q.Currencies.Asia
  )
where

import           Q.Currency

-- | Syrian Pounds
syp :: Currency
syp = Currency {
    cName           = "Syrian pounds"
  , cCode           = "SYP"
  , cIsoCode        = 4217
  , cFracsPerUnit   = 100
}
