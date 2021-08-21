module Q.Currencies.Asia
  (
    module Q.Currencies.Asia
  )
where

import           Q.Currency

-- | Syrian Pounds
syp :: Currency
syp = Currency {
    _name           = "Syrian pounds"
  , _code           = "SYP"
  , _isoCode        = 4217
  , _fracsPerUnit   = 100
}
