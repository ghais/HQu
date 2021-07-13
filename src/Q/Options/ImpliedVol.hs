module Q.Options.ImpliedVol
  (
      VolShift(..)
    , VolType(..)
    , euImpliedVol
  )
  where

import Q.Types
import           GHC.Generics (Generic)
import Data.Vector.Storable (Storable)
import qualified Q.Options.ImpliedVol.Normal as Bacherlier
import qualified Q.Options.ImpliedVol.LetsBeRational as B76




newtype VolShift = VolShift Double
  deriving stock (Generic, Eq, Show, Read, Ord)
  deriving newtype (Num, Fractional, Real, RealFrac, RealFloat, Floating, Storable)

data VolType = Normal
             | LogNormal
             | ShiftedLogNormal VolShift
             deriving stock (Generic, Eq, Show, Read)


euImpliedVol :: VolType -> OptionType -> Forward -> Strike -> YearFrac -> DF -> Premium -> Vol
euImpliedVol Normal cp f k t df premium =
  let r = rateFromDiscount t df
  in Bacherlier.euImpliedVol cp f k t r premium
euImpliedVol (ShiftedLogNormal (VolShift shift)) cp f k t df premium =
  let r = rateFromDiscount t df
  in B76.euImpliedVol cp (f + Forward shift) (k + Strike shift) t r premium
euImpliedVol LogNormal cp f k t df p = euImpliedVol (ShiftedLogNormal 0) cp f k t df p



