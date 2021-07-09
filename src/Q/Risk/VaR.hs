module Q.Risk.VaR where

import Statistics.Autocorrelation
import Data.Csv
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified Numeric.GSL.Statistics as GSL

y :: IO [Double]
y = do
  bytes <- B.readFile "/home/ghais/Downloads/TableF20-1.csv"
  return $ case (decode HasHeader bytes) of
    Left err -> error err
    Right v -> V.toList (V.map fromOnly v)

ar = do
  xs <- demean . VS.fromList <$> y
  let xs2 = VS.map (^2) xs
  return $ GSL.lag1auto  xs2
  where demean xs = let mean = GSL.mean xs
                    in VS.map (\x -> x - mean) xs
