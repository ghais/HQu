{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes#-}

module Q.Options.ImpliedVol
  (
      module Q.Types
    , module Q.Options
    , LogRelStrike(..)
    , AbsRelStrike(..)
    , MoneynessForwardStrike(..)
    , LogMoneynessForwardStrike(..)
    , MoneynessSpotStrike(..)
    , LogMoneynessSpotStrike(..)
    , VolShift(..)
    , VolType(..)
    , euImpliedVol
  )
  where

import Q.Types
import Q.Options
import Q.Options.BlackScholes
import           GHC.Generics (Generic)
import Data.Vector.Storable (Storable)
import qualified Q.Options.ImpliedVol.Normal as Bacherlier
import qualified Q.Options.ImpliedVol.LetsBeRational as B76


newtype AbsRelStrike = AbsRel Double
  deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype LogRelStrike = LogRel Double
  deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype MoneynessForwardStrike = MoneynessForward Double
  deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype LogMoneynessForwardStrike = LogMoneynessForward Double
  deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype MoneynessSpotStrike = MoneynessSpot Double
  deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

newtype LogMoneynessSpotStrike = LogMoneynessSpot Double
  deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)


newtype VolShift = VolShift Double
  deriving (Generic, Eq, Show, Read, Ord, Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

data VolType = Normal
             | LogNormal
             | ShiftedLogNormal VolShift
             deriving (Generic, Eq, Show, Read)


euImpliedVol :: VolType -> OptionType -> Forward -> Strike -> YearFrac -> DF -> Premium -> Vol
euImpliedVol Normal cp f k t df premium =
  let r = rateFromDiscount t df
  in Bacherlier.euImpliedVol cp f k t r premium
euImpliedVol (ShiftedLogNormal (VolShift shift)) cp f k t df premium =
  let r = rateFromDiscount t df
  in B76.euImpliedVol cp (f + Forward shift) (k + Strike shift) t r premium
euImpliedVol LogNormal cp f k t df p = euImpliedVol (ShiftedLogNormal 0) cp f k t df p



