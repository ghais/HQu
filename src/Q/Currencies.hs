module Q.Currencies
  (
    fromCode
  )
  where

import Q.Currencies.America
import Q.Currencies.Asia
import Q.Currencies.Europe
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Q.Currency

allCurrencies :: M.Map String Currency
allCurrencies = M.fromList [ ("usd", usd), ("USD", usd)
                           , ("cad", cad), ("CAD", cad)
                           , ("chf", chf), ("CHF", chf)
                           , ("eur", eur), ("EUR", eur)
                           , ("gbp", gbp), ("GBP", gbp)
                           , ("syp", syp), ("SYP", syp)
                           ]
fromCode :: String -> Currency
fromCode cny = fromMaybe (Currency cny cny 0 0) (M.lookup cny allCurrencies)
