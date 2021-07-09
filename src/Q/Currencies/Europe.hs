module Q.Currencies.Europe
  (
    module Q.Currencies.Europe
  )
where

import           Q.Currency

-- | Swiss france
chf :: Currency
chf = Currency {
    cName           = "Swiss franc"
  , cCode           = "CHF"
  , cIsoCode        = 756
  , cFracsPerUnit   = 100
  }

-- | European Euro
eur :: Currency
eur = Currency {
    cName           = "European Euro"
  , cCode           = "EUR"
  , cIsoCode        = 978
  , cFracsPerUnit   = 100
  }

-- | British pound sterling
gbp :: Currency
gbp = Currency {
    cName           = "British pound sterling"
  , cCode           = "GBP"
  , cIsoCode        = 826
  , cFracsPerUnit   = 100
  }
