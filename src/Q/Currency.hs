module Q.Currency
  (
    module Q.Currency
  )
where
import Control.Lens.TH

-- | Currency specification
data Currency = Currency {
  -- | currency name, e.g. "U.S. dollar"
    _name           :: String
    -- | ISO 4217 three-letter code, e.g. "USD"
  , _code           :: String
    -- | ISO 4217 numeric code, e.g. 840
  , _isoCode        :: Integer
    -- | number of fractionary parts in a unit
  , _fracsPerUnit   :: Integer
  } deriving stock (Eq)

$(makeLenses ''Currency)
instance Show Currency where
  showsPrec _ x s = _code x ++ s

instance Ord Currency where
  c1 <= c2 = _code c1 <= _code c2
  
