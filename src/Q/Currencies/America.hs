module Q.Currencies.America
  (
    module Q.Currencies.America
  )
where

import           Q.Currency

-- | Canadian dollar
cad :: Currency
cad = Currency {
    _name           = "Canadian dollar"
  , _code           = "CAD"
  , _isoCode        = 124
  , _fracsPerUnit   = 100
}

-- | U.S. dollar
usd :: Currency
usd = Currency {
    _name           = "U.S. dollar"
  , _code           = "USD"
  , _isoCode        = 840
  , _fracsPerUnit   = 100
}
