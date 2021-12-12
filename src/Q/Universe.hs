module Q.Universe where
import Q.Universe.IR
import Q.Universe.FX
import Q.Universe.EQ
import Control.Lens.TH
import qualified Data.Map as M
import Q.Currency (Currency)
import Control.Lens


newtype Asset = Asset String deriving newtype (Eq, Show, Read, Ord)

data Universe = Universe
  {
    _irAssets :: M.Map Currency IR
  , _eqAssets :: M.Map Asset EqAsset 
  , _fxAssets :: M.Map (Currency, Currency) FX
  }

makeLenses ''Universe


irAsset :: Currency -> Universe -> Maybe IR
irAsset cny u = M.lookup cny (view irAssets u)

fxAsset :: Currency -> Currency -> Universe -> Maybe FX
fxAsset cny1 cny2 u | cny1 < cny2 = M.lookup (cny1, cny2) (view fxAssets u)
                    | otherwise   = M.lookup (cny2, cny1) (view fxAssets u)

eqAsset :: Asset -> Universe -> Maybe EQ
eqAsset asset u = M.lookup asset (view eqAssets u)


