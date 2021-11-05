{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Q.Interpolation where
import           Data.Coerce (Coercible, coerce)
import qualified Numeric.GSL.Interpolation as GSL
import qualified Data.Vector.Storable as V
import Data.Kind
import Q.Types

class (Ord x, Coercible y Double, Coercible Double y) => Interpolator a x y where
  interpolate         :: a -> x -> y
  interpolateConstExt :: a -> x -> y
  derivative       :: a -> x -> Derivative x y
  secondDerivative :: a -> x -> Derivative x y
  xMin :: a -> (x, y)
  xMax :: a -> (x, y)

  interpolateConstExt a x = if x > x0 then y0 else interpolate a x
    where (x0, y0) = xMin a


type Interpolation :: Type -> Type -> Type
data Interpolation x y where
  Linear :: (Coercible x Double, Coercible y Double) =>  V.Vector Double -> V.Vector Double -> Interpolation x y
  CubicSpline :: (Coercible x Double, Coercible y Double) =>  V.Vector Double -> V.Vector Double -> Interpolation x y
  AkimaSpline :: (Coercible x Double, Coercible y Double) =>  V.Vector Double -> V.Vector Double -> Interpolation x y
  LogInterp :: (Coercible x Double, Coercible y Double) =>  Interpolation x Double -> Interpolation x y


linearInterpolator :: (Coercible x Double, Coercible y Double) => [x] -> [y] -> Interpolation x y
linearInterpolator xs ys = Linear (V.fromList (coerce xs))  (V.fromList (coerce ys))
cubicSplineInterpolator :: (Coercible x Double, Coercible y Double) => [x] -> [y] -> Interpolation x y
cubicSplineInterpolator xs ys = CubicSpline (V.fromList (coerce xs))  (V.fromList (coerce ys))
akimaSplineInterpolator :: (Coercible x Double, Coercible y Double) => [x] -> [y] -> Interpolation x y
akimaSplineInterpolator xs ys = AkimaSpline (V.fromList (coerce xs))  (V.fromList (coerce ys))

logLinearInterpolator :: (Coercible x Double, Coercible y Double) => [x] -> [y] -> Interpolation x y
logLinearInterpolator xs ys = let logYs = map log (coerce ys)
                                  interpolator = linearInterpolator  xs logYs
                              in LogInterp interpolator

linear :: (Coercible x Double, Coercible y Double, Floating y, Ord x) => [x] -> [y] -> x -> y
linear xs ys = interpolate (linearInterpolator xs ys)
cubic :: (Coercible x Double, Coercible y Double, Floating y, Ord x) => [x] -> [y] -> x -> y
cubic xs ys = interpolate (cubicSplineInterpolator xs ys)
akima :: (Coercible x Double, Coercible y Double, Floating y, Ord x) => [x] -> [y] -> x -> y
akima xs ys = interpolate (akimaSplineInterpolator xs ys)

instance (Coercible y Double, Coercible Double y, Floating y, Ord x) =>  Interpolator (Interpolation x y) x y where
  xMin (Linear xs ys) = coerce (V.head xs, V.head ys)
  xMin (LogInterp f) = let (x0, y0::Double) = xMin f in (x0, coerce (exp y0))
  xMin (CubicSpline xs ys) = coerce (V.head xs, V.head ys)
  xMin (AkimaSpline xs ys) = coerce (V.head xs, V.head ys)

  xMax (Linear xs ys) = coerce (V.last xs, V.last ys)
  xMax (LogInterp f) = let (x0, y0::Double) = xMax f in (x0, coerce (exp y0))
  xMax (CubicSpline xs ys) = coerce (V.last xs, V.last ys)
  xMax (AkimaSpline xs ys) = coerce (V.last xs, V.last ys)

  interpolate (Linear  xs ys) x = coerce $ GSL.evaluateV GSL.Linear xs ys (coerce x)
  interpolate (LogInterp f) x = coerce $ exp y where
    y = interpolate f x :: Double
  interpolate (CubicSpline  xs ys) x = coerce $ GSL.evaluateV GSL.CSpline  xs ys (coerce x)
  interpolate (AkimaSpline  xs ys) x = coerce $ GSL.evaluateV GSL.Akima xs ys (coerce x)

  derivative (Linear  xs ys) x = Derivative $ GSL.evaluateDerivativeV  GSL.Linear xs ys (coerce x)
  derivative interp@(LogInterp f) x = Derivative $ coerce y * df
    where
      y = interpolate interp x ::y
      (Derivative df) = derivative f x::Derivative x Double
  derivative (CubicSpline  xs ys) x = Derivative $ GSL.evaluateDerivativeV  GSL.CSpline  xs ys (coerce x)
  derivative (AkimaSpline  xs ys) x = Derivative $ GSL.evaluateDerivativeV  GSL.Akima xs ys (coerce x)

  secondDerivative (Linear  xs ys) x = Derivative $ GSL.evaluateDerivative2V  GSL.Linear xs ys (coerce x)
  secondDerivative interp@(LogInterp f) x = Derivative $ dx * dfx + coerce finterp * d2fdx2
    where (Derivative dfx) =  derivative f x:: Derivative x Double
          (Derivative dx) = derivative interp x :: Derivative x y
          (Derivative d2fdx2) = secondDerivative f x :: Derivative x Double
          finterp = interpolate interp x::y

  secondDerivative (CubicSpline  xs ys) x = Derivative $ coerce $ GSL.evaluateDerivative2V  GSL.CSpline  xs ys (coerce x)
  secondDerivative (AkimaSpline  xs ys) x = Derivative $ coerce $ GSL.evaluateDerivative2V  GSL.Akima xs ys (coerce x)
