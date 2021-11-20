{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE DerivingVia #-}

module Q.Types (
    ISIN
  , CUSIP
  , Observables1(..)
  , Observables2(..)
  , Observables3(..)
  , Observables4(..)
  , Observables5(..)
  , OptionType(..)
  , Cash(..)
  , Spot(..)
  , Obs1(..)
  , Obs2(..)
  , Obs3(..)
  , Obs4(..)
  , Obs5(..)
  , Strike(..)
  , LogRelStrike(..)
  , AbsRelStrike(..)
  , MoneynessForwardStrike(..)
  , LogMoneynessForwardStrike(..)
  , MoneynessSpotStrike(..)
  , LogMoneynessSpotStrike(..)
  , Forward(..)
  , Premium(..)
  , Delta(..)
  , Vega(..)
  , Gamma(..)
  , Theta(..)
  , Rho (..)
  , Kappa (..)
  , Expiry(..)
  , YearFrac(..)
  , Rate(..)
  , DF(..)
  , Alpha(..)
  , Beta(..)
  , M(..)
  , Sigma(..)
  , V(..)
  , Phi(..)
  , P(..)
  , C(..)
  , VTil(..)
  , Lambda(..)
  , Nu(..)
  , Eta(..)
  , Vol(..)
  , Var(..)
  , TotalVar(..)
  , TimeScaleable(..)
  , Tolerance(..)
  , Derivative(..)
  , cpi
  , discountFactor
  , discount
  , undiscount
  , rateFromDiscount
  , totalVarToVol
  , volToTotalVar
  , ($*$)
  , ($/$)
  , ($+$)
  , ($-$)
  ) where

import qualified Data.ByteString as B
import           Data.Coerce
import           Data.Csv (FromField (..), ToField (..))
import           Data.Kind
import           Data.Time
import           Foreign (Storable)
import           GHC.Generics (Generic)
import           Q.Time ()
import Data.Semigroup
-- | Type for Put or Calls
data OptionType  = Put | Call deriving (Generic, Eq, Show, Read, Bounded)
instance Enum OptionType where
  succ Call = Put
  succ Put  = Call

  pred = succ
  toEnum x = if signum x == 1 then Call else Put
  fromEnum Call = 1
  fromEnum Put  = -1


cpi Call = 1
cpi Put  = -1

newtype ISIN = ISIN String deriving stock (Generic, Eq, Show, Read, Ord)
newtype CUSIP = CUSIP String deriving stock (Generic, Eq, Show, Read, Ord)


newtype Cash     = Cash    Double deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
                                  deriving Semigroup via Sum Double
                                  deriving Monoid via Sum Double

newtype Spot     = Spot    Double deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Forward  = Forward Double deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)             
newtype Strike   = Strike  Double deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
                                  deriving Semigroup via Sum Double
                                  deriving Monoid via Sum Double

newtype AbsRelStrike = AbsRel Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
  deriving Semigroup via Sum Double
  deriving Monoid via Sum Double
newtype LogRelStrike = LogRel Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
  deriving Semigroup via Sum Double
  deriving Monoid via Sum Double

newtype MoneynessForwardStrike = MoneynessForward Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
  deriving Semigroup via Sum Double
  deriving Monoid via Sum Double

newtype LogMoneynessForwardStrike = LogMoneynessForward Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
  deriving Semigroup via Sum Double
  deriving Monoid via Sum Double

newtype MoneynessSpotStrike = MoneynessSpot Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
  deriving Semigroup via Sum Double
  deriving Monoid via Sum Double

newtype LogMoneynessSpotStrike = LogMoneynessSpot Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
  deriving Semigroup via Sum Double
  deriving Monoid via Sum Double

newtype Tolerance = Tolerance    Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

type Derivative :: Type -> Type -> Type
newtype Derivative x y where
  Derivative :: Double -> Derivative x y

deriving stock instance Show (Derivative x y)
deriving stock instance Eq (Derivative x y)
deriving stock instance Generic (Derivative x y)
deriving stock instance Ord (Derivative x y)
deriving newtype instance Num (Derivative x y)
deriving newtype instance Fractional (Derivative x y)
deriving newtype instance Real (Derivative x y)
deriving newtype instance RealFrac (Derivative x y)
deriving newtype instance RealFloat (Derivative x y)
deriving newtype instance Floating (Derivative x y)
deriving newtype instance Storable (Derivative x y)



($*$) :: (Coercible a Double, Coercible b Double) => a -> b -> a
x1 $*$ x2 = coerce $ (coerce x1::Double) * (coerce x2::Double)

($/$) :: (Coercible a Double, Coercible b Double) => a -> b -> a
x1 $/$ x2 = coerce $ (coerce x1::Double) / (coerce x2::Double)

($+$) :: (Coercible a Double, Coercible b Double) => a -> b -> a
x1 $+$ x2 = coerce $ (coerce x1::Double) + (coerce x2::Double)

($-$) :: (Coercible a Double, Coercible b Double) => a -> b -> a
x1 $-$ x2 = coerce $ (coerce x1::Double) - (coerce x2::Double)

-- Later on i should add roll.
newtype Expiry   = Expiry   Day    deriving (Generic, Eq, Show, Read, Ord)

newtype Premium  = Premium  Double deriving stock (Generic, Eq, Show, Ord)
                                   deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Delta    = Delta    Double deriving stock (Generic, Eq, Show, Ord)
                                   deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Vega     = Vega     Double deriving stock (Generic, Eq, Show, Ord)
                                   deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Gamma    = Gamma    Double deriving stock (Generic, Eq, Show, Ord)
                                   deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Theta    = Theta    Double deriving stock (Generic, Eq, Show, Ord)
                                   deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Rho      = Rho      Double deriving stock (Generic, Eq, Show, Ord)
                                   deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype Kappa    = Kappa    Double deriving stock (Generic, Eq, Show, Ord)
                                   deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)


newtype YearFrac = YearFrac {unYearFrac:: Double} deriving stock (Generic, Eq, Show, Ord)
                                                 deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype Rate     = Rate Double deriving stock (Generic, Eq, Show, Ord, Read)
                               deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype DF       = DF   Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype Alpha  = Alpha  Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Beta   = Beta   Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype M      = M      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Sigma  = Sigma  Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)

newtype V      = V      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Phi    = Phi    Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype P      = P      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype C      = C      Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype VTil   = VTil   Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Lambda = Lambda Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Nu     = Nu     Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)
newtype Eta    = Eta    Double deriving stock (Generic, Eq, Show, Ord)
                               deriving newtype (Num, Fractional, Floating)

discountFactor :: YearFrac -> Rate -> DF
discountFactor (YearFrac t) (Rate r) = DF $ exp ((-r) * t)
discount (DF df) p = p * df
undiscount (DF df) p = p / df

rateFromDiscount (YearFrac t) (DF df) = Rate $ - (log df) / t

newtype Vol      = Vol       Double deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
newtype Var      = Var       Double deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)
-- | (\w(S_0, K, T) = \sigma_{BS}(S_0, K, T)T \)
newtype TotalVar = TotalVar  Double deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

totalVarToVol (TotalVar v) (YearFrac t) = Vol $ sqrt (v / t)
volToTotalVar (Vol sigma) (YearFrac t) = TotalVar $ sigma * sigma * t

instance FromField OptionType where
  parseField s | (s == "C" || s == "c") = pure Call
               | (s == "P" || s == "p")  = pure Put
instance ToField OptionType where
  toField Call = toField ("C"::B.ByteString)
  toField Put  = toField ("P"::B.ByteString)

instance FromField Spot where
  parseField s = Spot <$> parseField s
instance ToField Spot where
  toField (Spot k) = toField k

instance FromField Cash where
  parseField s = Cash <$> parseField s
instance ToField Cash where
  toField (Cash k) = toField k


instance FromField Strike where
  parseField s = Strike <$> parseField s
instance ToField Strike where
  toField (Strike k) = toField k

instance FromField Expiry where
  parseField s = Expiry <$> parseField s
instance ToField   Expiry where
  toField (Expiry k) = toField k

instance FromField Premium where
    parseField s = Premium <$> parseField s
instance ToField   Premium  where
  toField (Premium k) = toField k

instance FromField Delta where
    parseField s = Delta <$> parseField s
instance ToField   Delta  where
  toField (Delta k) = toField k

instance FromField Vega where
    parseField s =  Vega <$> parseField s
instance ToField   Vega  where
  toField (Vega k) = toField k

instance FromField Gamma where
    parseField s =  Gamma <$> parseField s
instance ToField   Gamma  where
  toField (Gamma k) = toField k

instance FromField YearFrac where
    parseField s =  YearFrac <$> parseField s
instance ToField   YearFrac  where
  toField (YearFrac k) = toField k

instance FromField Rate where
    parseField s =  Rate <$> parseField s
instance ToField   Rate  where
  toField (Rate k) = toField k


instance FromField Vol where
    parseField s =  Vol <$> parseField s
instance ToField   Vol  where
  toField (Vol k) = toField k

-- | Represents concepts that scale as a function of time such as 'Vol'
class TimeScaleable a where
  scale :: YearFrac -> a -> a

instance TimeScaleable Double where
  scale (YearFrac t) y = y * t

instance TimeScaleable Rate where
  scale (YearFrac t) (Rate r)  = Rate $ r * t
instance TimeScaleable Vol where
  scale (YearFrac t) (Vol sigma)  = Vol $ sigma * sqrt t


-- | Single-observable container.
data Observables1 = Observables1 {-# UNPACK #-} !Double
-- | Two observable container.
data Observables2 = Observables2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
-- | Three observable container.
data Observables3 = Observables3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                                 {-# UNPACK #-} !Double
-- | Four observable container.
data Observables4 = Observables4 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                                 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
-- | Five observable container.
data Observables5 = Observables5 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                                 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                                 {-# UNPACK #-} !Double

class Obs1 a where
    get1 :: a -> Double

class (Obs1 a) => Obs2 a where
    get2 :: a -> Double

class (Obs2 a) => Obs3 a where
    get3 :: a -> Double

class (Obs3 a) => Obs4 a where
    get4 :: a -> Double

class (Obs4 a) => Obs5 a where
    get5 :: a -> Double

instance Obs1 Observables1 where
    get1 (Observables1 x) = x
    {-# INLINE get1 #-}

instance Obs1 Observables2 where
    get1 (Observables2 x _) = x
    {-# INLINE get1 #-}

instance Obs1 Observables3 where
    get1 (Observables3 x _ _) = x
    {-# INLINE get1 #-}

instance Obs1 Observables4 where
    get1 (Observables4 x _ _ _) = x
    {-# INLINE get1 #-}

instance Obs1 Observables5 where
    get1 (Observables5 x _ _ _ _) = x
    {-# INLINE get1 #-}

instance Obs2 Observables2 where
    get2 (Observables2 _ x) = x
    {-# INLINE get2 #-}

instance Obs2 Observables3 where
    get2 (Observables3 _ x _) = x
    {-# INLINE get2 #-}

instance Obs2 Observables4 where
    get2 (Observables4 _ x _ _) = x
    {-# INLINE get2 #-}

instance Obs2 Observables5 where
    get2 (Observables5 _ x _ _ _) = x
    {-# INLINE get2 #-}

instance Obs3 Observables3 where
    get3 (Observables3 _ _ x) = x
    {-# INLINE get3 #-}

instance Obs3 Observables4 where
    get3 (Observables4 _ _ x _) = x
    {-# INLINE get3 #-}

instance Obs3 Observables5 where
    get3 (Observables5 _ _ x _ _) = x
    {-# INLINE get3 #-}

instance Obs4 Observables4 where
    get4 (Observables4 _ _ _ x) = x
    {-# INLINE get4 #-}

instance Obs4 Observables5 where
    get4 (Observables5 _ _ _ x _) = x
    {-# INLINE get4 #-}

instance Obs5 Observables5 where
    get5 (Observables5 _ _ _ _ x) = x
    {-# INLINE get5 #-}
