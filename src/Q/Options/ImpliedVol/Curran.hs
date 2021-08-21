module Q.Options.ImpliedVol.Curran where

import Q.Options.Pricing.Curran
import           Q.Options.Black76
import Q.Types
import qualified Numeric.RootFinding as R
import Control.Monad.Except
import Data.Either (fromRight)
import Numeric.IEEE (IEEE(nan))
import           Data.Default.Class (Default (..))



instance Default RootFinding where
  def = RootFinding 300 (R.RelTol 1e-5) (0.0001, 0.5, 10)

data RootFinding = RootFinding
        Int                      -- ^ Maximum number of iterations.
        R.Tolerance              -- ^ Tolerance (relative or absolute)
        (Double, Double, Double) -- ^ Triple of @(low bound, initial
                                 --   guess, upper bound)@. If initial
                                 --   guess if out of bracket middle
                                 --   of bracket is taken as.



curranImpliedVol :: RootFinding -> OptionType -> Forward -> Strike -> YearFrac -> DF -> YearFrac -> Int -> Int -> Double -> Premium -> CurranMonad Vol
curranImpliedVol (RootFinding maxIter tol bracket) cp f k ttm df firstMonitor nMonitor nFixed sFixed (Premium p) =
  let priceVol vol = p' - p where
        (Premium p') = fromRight (Premium nan) (runExcept (asianOption b76 firstMonitor nMonitor nFixed sFixed cp k))
        b76 = Black76 f df ttm (Vol vol)
      (lb, _, ub) = bracket
      root = R.ridders (R.RiddersParam (fromEnum maxIter) tol) (lb, ub) priceVol
  in case root of (R.Root vol)   -> return $ Vol vol
                  R.NotBracketed -> throwError $ NoImpliedVol  "not bracketed"
                  R.SearchFailed -> throwError $ NoImpliedVol "search failed"



