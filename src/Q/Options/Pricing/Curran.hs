module Q.Options.Pricing.Curran where
import           Control.Monad.Except
import           Q.Options (vPremium)
import           Q.Options.Black76
import qualified Q.SortedVector as SV
import           Q.Types
import GHC.Base (coerce)
import qualified Numeric.GSL.Root as Root


data CurranError = NegativeVol Vol
                 | InvalidParameters String deriving stock (Show, Eq)

type CurranMonad = Either CurranError

asianOption :: Black76  -> SV.SortedVector YearFrac -> Int -> Double -> OptionType -> Strike -> CurranMonad Premium
asianOption b76@Black76{..} monitoringTimes nFixed sFixed cp (Strike k) = do
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
    handleNormalCase b76 monitoringTimes nFixed sFixed cp (Strike k)
  where checkPreconditions = do
           when (SV.null monitoringTimes) $
             throwError (InvalidParameters "No monitoring times specified")
           when (SV.head monitoringTimes > b76T) $
             throwError (InvalidParameters "Time to first monitoring point cannot be after the B76 tenor")
           when (nFixed < 0) $
             throwError (InvalidParameters "Num fixed points cannot be negative")
           when (sFixed > 0 && nFixed == 0) $
             throwError (InvalidParameters "No points are fixed so running sum cannot be greater than 0")
           when (b76T == 0 && nFixed /=  SV.length monitoringTimes) $
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

        isLastFixing = SV.length monitoringTimes - nFixed == 1
        handleLastFixing = let kHat = Strike $ nMonitoringTimes * k - sFixed
                           in return $ vPremium $ euOption b76 cp kHat

        nRemainingFixings = fromIntegral (SV.length monitoringTimes - nFixed)
        nMonitoringTimes = fromIntegral $ SV.length monitoringTimes
        nFixingsSoFar    = fromIntegral nFixed
        (Forward f0) = b76F


handleNormalCase :: Black76  -> SV.SortedVector YearFrac -> Int -> Double -> OptionType -> Strike -> CurranMonad Premium
handleNormalCase b76@Black76{..} monitoringTimes nFixed sFixed cp (Strike k) = do
  let nRemainingFixings = fromIntegral $ SV.length monitoringTimes - nFixed
      nMonitoringTimes = fromIntegral $ SV.length monitoringTimes
      kEffective = if 0 < nRemainingFixings then (k * nMonitoringTimes - sFixed) / nRemainingFixings else k
      largeVolThreshold = 100
      (Forward f0) = b76F
  if largeVolThreshold < b76Vol then
    return $ Premium $ discount b76DF (if cp == Call then f0 else kEffective)
  else
    let params = foldl updateParams  (curranParams b76 nRemainingFixings) (drop nFixed (SV.toList monitoringTimes))
        updateParams params@Params{..} (YearFrac tt_j) =
           params {
                    timeSum = YearFrac timeSum'
                  , weightedTimeSum = weightedTimeSum'
                  , gammaI = (YearFrac tt_j, gamma) : gammaI
                  , j = j + 1
                  }
           where weightedTimeSum'    = weightedTimeSum + j * tt_j
                 (YearFrac timeSum') = timeSum + YearFrac tt_j
                 gamma = sigmaSquaredNorm * (timeSum' + (nRemainingFixings - j) * tt_j)
        muG = log f0 - 0.5 * sigmaSquaredNorm params * coerce (timeSum params)
        sigma2G = sigmaSquaredNorm params / nRemainingFixings * ((2 * nRemainingFixings + 1) * coerce (timeSum params) - 2 * weightedTimeSum params)
        sigmaG = sqrt sigma2G
    in  undefined

data Params = Params
  {
    gammaI :: ![(YearFrac, Double)]
  , timeSum :: !YearFrac
  , weightedTimeSum :: !Double
  , sigmaSquaredNorm :: !Double
  , j :: Double
  }

curranParams :: Black76 -> Double -> Params
curranParams (Black76 _ _ _ (Vol sigma)) nRemainingFixings = Params [] 0 0 (sigma * sigma / nRemainingFixings) 1



lowerBounds :: Black76 -> Params -> Double -> Double -> Double -> Double -> Double
lowerBounds b76@Black76{..} params@Params{..} muG sigma2G kEff nFixings =
  let nuI = map (calcNuI b76 muG sigma2G) gammaI
      f x = let expect = foldr (\((_, gamma_i), nu_i ) expectation  -> expectation + nu_i * (x**gamma_i/sigma2G))  0 (zip gammaI nuI)
            in expect / nFixings - kEff
      f' x = let deriv = foldr (\((_, gamma_i), nu_i ) derivative -> derivative + nu_i * gamma_i * (x**gamma_i/sigma2G))
             in deriv / (x * nFixings *sigma2G)
  in undefined

calcNuI :: Black76 -> Double -> Double -> (YearFrac, Double) -> Double
calcNuI (Black76 (Forward f) _ _  (Vol sigma)) muG sigma2G (YearFrac t, gamma) =
  let mu = log f - 0.5 * sigma * sigma * t
      sigma2 = sigma * sigma * t
  in exp $ mu - muG * gamma / sigma2G + 0.5 * (sigma2 - gamma * gamma / sigma2G)
