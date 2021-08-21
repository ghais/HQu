module Q.Currencies.Europe
  (
    module Q.Currencies.Europe
  )
where

import           Q.Currency

-- | Swiss france
chf :: Currency
chf = Currency {
    _name           = "Swiss franc"
  , _code           = "CHF"
  , _isoCode        = 756
  , _fracsPerUnit   = 100
  }

-- | European Euro
eur :: Currency
eur = Currency {
    _name           = "European Euro"
  , _code           = "EUR"
  , _isoCode        = 978
  , _fracsPerUnit   = 100
  }

-- | British pound sterling
gbp :: Currency
gbp = Currency {
    _name           = "British pound sterling"
  , _code           = "GBP"
  , _isoCode        = 826
  , _fracsPerUnit   = 100
  }
