module Q.Options.Pricing.Curran where
import           Control.Monad.Except
import           Q.Options (vPremium)
import           Q.Options.Black76
import Numeric.RootFinding
import           Q.Types
import           Statistics.Distribution (cumulative)
import           Statistics.Distribution.Normal (standard)



data CurranError = NegativeVol Vol
                 | InvalidParameters String
                 | NoSolution String
                 | NoImpliedVol String deriving stock (Show, Eq)

type CurranMonad = Except CurranError

asianOption :: Black76  -> YearFrac -> Int -> Int -> Double -> OptionType -> Strike -> CurranMonad Premium
asianOption b76@Black76{..} firstMonitor nMonitor nFixed sFixed cp (Strike k) = do
  checkPreconditions
  if isPastExpiry then
    return 0.0
  else if isOnExpiry then
    handleOnExpiry
  else if isNearZeroVol then
    handleNearZeroVol
  else if isDefinitlyITM then
    handleDefinitlyITM
  else if isLastFixing then
    handleLastFixing
  else
    handleNormalCase b76 firstMonitor nMonitoringTimes nFixingsSoFar sFixed cp (Strike kEffective)
  where checkPreconditions = do
           when (nMonitor <= 0) $
             throwError (InvalidParameters "No monitoring times specified")
           when (firstMonitor > b76T) $
             throwError (InvalidParameters "Time to first monitoring point cannot be after the B76 tenor")
           when (nFixed < 0) $
             throwError (InvalidParameters "Num fixed points cannot be negative")
           when (sFixed > 0 && nFixed == 0) $
             throwError (InvalidParameters "No points are fixed so running sum cannot be greater than 0")
           when (b76T == 0 && nFixed /=  nMonitor) $
              throwError (InvalidParameters "All points shuold be fixed")

        isPastExpiry = b76T < 0

        isOnExpiry = b76T == 0 && nRemainingFixings == 0
        handleOnExpiry = return $ Premium $ max 0 (cpi cp * sFixed / nMonitoringTimes - k)

        isNearZeroVol = abs b76Vol < 1e-6
        handleNearZeroVol = let finalAvg = (sFixed + nRemainingFixings * f0) / nMonitoringTimes
                            in return $ Premium $ discount b76DF (max 0 (cpi cp * (finalAvg - k)))

        isDefinitlyITM = nFixed > 0 && f0 >= nMonitoringTimes * k / nFixingsSoFar
        handleDefinitlyITM = let finalAvg = (sFixed + nRemainingFixings * f0) / nMonitoringTimes
                             in return $ Premium $ discount b76DF (max 0 (cpi cp * (finalAvg - k)))

        isLastFixing = nMonitor - nFixed == 1
        handleLastFixing = let kHat = Strike $ nMonitoringTimes * k - sFixed
                           in return $ vPremium $ euOption b76 cp kHat

        nRemainingFixings = fromIntegral (nMonitor - nFixed)
        nMonitoringTimes = fromIntegral nMonitor
        nFixingsSoFar    = fromIntegral nFixed
        (Forward f0) = b76F
        kEffective = if 0 < nRemainingFixings then
                        (k * nMonitoringTimes - sFixed) / nRemainingFixings
                     else k


handleNormalCase :: Black76  -> YearFrac -> Double -> Double -> Double -> OptionType -> Strike -> CurranMonad Premium
handleNormalCase b76@Black76{..} (YearFrac firstMonitor) n m sFixed cp (Strike k) = do
  let t1                = (ttm - firstMonitor) / (n - 1) * m  + firstMonitor
      p                 = n - m
      z                 = cpi cp
      (Vol v)           = b76Vol
      (Forward s)       = b76F
      dt                = if (n - m - 1) == 0 then ttm else (ttm - t1) / (n - m - 1)
      largeVolThreshold = 100
      (Forward f0)      = b76F
      (YearFrac ttm)    = b76T
      mu                = ((-0.5) * v * v ) * (t1 + ttm) * 0.5
      tx                = t1 + dt * ((n - m) - 1) * (2 * (n - m) - 1) / (6 * (n - m));
      vx = v * sqrt(t1 + dt * (p - 1) * (2 * p - 1) / (6 * p))
      my = log(s) + (-v * v * 0.5) * (t1 + (p - 1) * dt / 2);

      arg sHat i        = let ti  = t1 + (i - 1) * dt
                              mui = (-0.5 * v * v) * ti;
                              txi = t1 + dt * (i - 1) * (1 - i / (2 * (n - m)))
                          in (txi, mui + (log(sHat / s) - mu) * txi / tx + 0.5 * v * v * (ti - txi * txi / tx))
      f  0   = (-k)
      f sHat = let argis =  map (arg sHat) [1.0..(n - m)]
                   sum1  = foldr (\(_, argi) sum0 -> sum0 + exp argi) 0 argis
               in if isInfinite sum1 then
                    -1
                  else s * sum1 / (n - m) - k
      f' 0 = 0
      f' sHat = let argis = map (arg sHat) [1.0..(n - m)]
                    sum1  = foldr (\(txi, argi) sum0 -> sum0 + (exp argi) * txi / tx) 0 argis
                in (s / sHat) * sum1 / (n - m);
  if largeVolThreshold < b76Vol then
    return $ Premium $ discount b76DF (if cp == Call then f0 else k)
  else
    let r =  newtonRaphson (NewtonParam 100 (AbsTol 1e-6)) (0.0001 * k * k, 0, k) (\x -> (f x, f' x))
    in case r of
      Root lb -> if lb < 0 then
                   return $ Premium $ discount b76DF (if cp == Call then f0 else k)
                 else
                   let arg2 i    =  exp(myi + 0.5 * vi_2) * cumulative standard (z * ((my - log lb + vxi) / vx)) where
                         ti   = dt * i + t1 - dt;
                         vi_2 = v * v * (t1 + (i - 1) * dt);
                         vxi = v * v * (t1 + dt * ((i - 1) - i * (i - 1) / (2 * p)))
                         myi = log(s) + (-v * v * 0.5) * ti
                       sum1 = sum (map arg2 [1..p])
        in return $ Premium $ discount b76DF $ max 0 (z * p / n * (1/ p * sum1 - k * (cumulative standard (z * (my - log lb)/vx))))
      _ -> throwError $ NoSolution "for lower bound"

