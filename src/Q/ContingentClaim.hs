{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes, ApplicativeDo #-}

module Q.ContingentClaim where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Q.Types
import Data.Time
import qualified Data.Map as M

-- | A cash flow is a time and amount.
data CashFlow = CashFlow {
    cfTime :: LocalTime -- ^ The cash flow time.
  , cfAmount :: Double  -- ^ The cash flow amount.
} deriving (Show, Eq)

-- | Stop at time t and potentially apply n payouts up to the monitoring time.
data CCProcessor a = CCProcessor {
    monitorTime :: LocalTime                   -- ^ Stopping time.
  , payouts  :: [M.Map LocalTime a -> CashFlow] -- ^ list of payout functions at the stopping time.
}

-- | A claim contingent on some observable a.
newtype ContingentClaim a = ContingentClaim { unCC :: [CCProcessor a] }
-- ^ An example of an observable is a spot driven asset, such as a stock.

instance Monoid (ContingentClaim a) where
  mempty  = ContingentClaim []

-- | multipley a contingent claim by its notional.
multiplier :: Double -> ContingentClaim a -> ContingentClaim a
multiplier notional (ContingentClaim ccProcessors) = ContingentClaim $ map scale ccProcessors where
  scale CCProcessor{ .. } = CCProcessor monitorTime (map scaledPayout payouts)
  scaledPayout payout = fmap (\ (CashFlow t v) -> CashFlow t (notional * v)) payout

-- | Change direction of the portfolio
short :: ContingentClaim a -> ContingentClaim a
short = multiplier (-1)


instance Semigroup (ContingentClaim a) where
  c1 <> c2 = ContingentClaim $ combine (unCC c1) (unCC c2)
    where combine (cc1:ccs1) (cc2:ccs2)
            | monitorTime cc1 == monitorTime cc2 = let
                CCProcessor t mf  = cc1
                CCProcessor _ mf' = cc2 in
                CCProcessor t (mf++mf') : combine ccs1 ccs2
            | monitorTime cc1 > monitorTime cc2 = cc2 : combine (cc1:ccs1) ccs2
            | otherwise = cc1 : combine ccs1 (cc2:ccs2)
          combine cs1 cs2 = cs1 ++ cs2

type CCBuilder w r a =  WriterT w (Reader r) a

-- | Monitor an observable at the given time t.
monitor :: LocalTime -> CCBuilder (ContingentClaim a) (M.Map LocalTime a) a
monitor t = do
  tell $ ContingentClaim [CCProcessor t []] -- This step maintains the monitoring times.
  m <- ask                                   -- This step gets the market data
  return $ m M.! t                          -- This step evaluate the market data at time t.

-- | Pay an amount at a given time
pay :: forall a. LocalTime -> CCBuilder (ContingentClaim a) (M.Map LocalTime a) CashFlow -> ContingentClaim a
pay t x = stoppingTimes <> ContingentClaim [CCProcessor t [payout]] where
  stoppingTimes = runReader (execWriterT x) M.empty
  payout = let r = fst <$> runWriterT x
           in runReader r

