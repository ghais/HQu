{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}
module Q.Interpolation where
import           Data.Coerce (Coercible, coerce)
import           Data.Kind
import           Data.Ord
import qualified Data.SortedList as SortedList
import qualified Data.Vector.Storable as V
import           GHC.Generics
import qualified Numeric.GSL.Interpolation as GSL
import           Numeric.IEEE
import           Q.Types
import Data.Time

class (Ord x) => Interpolable x where
  interpDiff :: x -> x -> Double

instance (Coercible x Double, Ord x) => Interpolable x where
  interpDiff x1 x2 = coerce x1 - coerce x2


instance Interpolable Day where
  interpDiff d1 d2 = fromInteger (diffDays d2 d1)

class (Ord x, Coercible y Double, Coercible Double y) => Interpolator a x y where
  interpolate         :: a -> x -> y
  interpolateConstExt :: a -> x -> y
  derivative       :: a -> x -> Derivative x y
  secondDerivative :: a -> x -> Derivative x y
  xMin :: a -> (x, y)
  xMax :: a -> (x, y)

  interpolateConstExt a x = if x > x0 then y0 else interpolate a x
    where (x0, y0) = xMin a


data Continuity = LeftContinious | RightContinious deriving stock (Generic, Read, Show, Enum, Bounded)

type Interpolation :: Type -> Type -> Type
data Interpolation x y where
  Const             :: (Coercible x Double, Coercible y Double) => Double -> Interpolation x y
  Linear            :: (Coercible x Double, Coercible y Double) => V.Vector Double -> V.Vector Double -> Interpolation x y
  CubicSpline       :: (Coercible x Double, Coercible y Double) => V.Vector Double -> V.Vector Double -> Interpolation x y
  AkimaSpline       :: (Coercible x Double, Coercible y Double) => V.Vector Double -> V.Vector Double -> Interpolation x y
  LogInterp         :: (Coercible x Double, Coercible y Double) => Interpolation x Double -> Interpolation x y
  PieceWiseConstant :: Continuity -> SortedList.SortedList (x, y) -> Interpolation x y


constInterpolator :: (Coercible x Double, Coercible y Double) => y -> Interpolation x y
constInterpolator = Const . coerce

leftContinuousPieceWiseConstant :: (Ord x, Ord y) => [x] -> [y] -> Interpolation x y
leftContinuousPieceWiseConstant xs ys = PieceWiseConstant LeftContinious (SortedList.toSortedList (zip xs ys)) --todo nub
rightContinuousPieceWiseConstant :: (Ord x, Ord y) => [x] -> [y] -> Interpolation x y
rightContinuousPieceWiseConstant xs ys = PieceWiseConstant RightContinious (SortedList.toSortedList (zip xs ys)) --todo nub?

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
  xMin (Const y)                 = coerce  (negate maxFinite::Double, y)
  xMin (Linear xs ys)            = coerce (V.head xs, V.head ys)
  xMin (LogInterp f)             = let (x0, y0::Double) = xMin f in (x0, coerce (exp y0))
  xMin (CubicSpline xs ys)       = coerce (V.head xs, V.head ys)
  xMin (AkimaSpline xs ys)       = coerce (V.head xs, V.head ys)
  xMin (PieceWiseConstant _ xys) = firstElement xys

  xMax (Const y)                 = coerce  (maxFinite::Double, y)
  xMax (Linear xs ys)            = coerce (V.last xs, V.last ys)
  xMax (LogInterp f)             = let (x0, y0::Double) = xMax f in (x0, coerce (exp y0))
  xMax (CubicSpline xs ys)       = coerce (V.last xs, V.last ys)
  xMax (AkimaSpline xs ys)       = coerce (V.last xs, V.last ys)
  xMax (PieceWiseConstant _ xys) = lastElement xys

  interpolate (Const y) _                               = coerce y
  interpolate (Linear  xs ys) x                         = coerce $ GSL.evaluateV GSL.Linear xs ys (coerce x)
  interpolate (LogInterp f) x                           = coerce $ exp y where y = interpolate f x :: Double
  interpolate (CubicSpline  xs ys) x                    = coerce $ GSL.evaluateV GSL.CSpline  xs ys (coerce x)
  interpolate (AkimaSpline  xs ys) x                    = coerce $ GSL.evaluateV GSL.Akima xs ys (coerce x)
  interpolate (PieceWiseConstant LeftContinious xys)  x = snd $ firstElement (SortedList.filter (\(x0, _) -> x0 >= x) xys)
  interpolate (PieceWiseConstant RightContinious xys) x = if x >= lastX then
                                                            lastY
                                                          else
                                                            snd $ firstElement (SortedList.filter (\(x0, _) -> x0 > x) xys)
    where (lastX, lastY) = lastElement xys

  derivative (Const _) _                = 0
  derivative (Linear  xs ys) x          = Derivative $ GSL.evaluateDerivativeV  GSL.Linear xs ys (coerce x)
  derivative interp@(LogInterp f) x     = Derivative $ coerce y * df
                                            where y               = interpolate interp x ::y
                                                  (Derivative df) = derivative f x::Derivative x Double
  derivative (CubicSpline  xs ys) x     = Derivative $ GSL.evaluateDerivativeV  GSL.CSpline  xs ys (coerce x)
  derivative (AkimaSpline  xs ys) x     = Derivative $ GSL.evaluateDerivativeV  GSL.Akima xs ys (coerce x)
  derivative (PieceWiseConstant _ _ ) _ = 0

  secondDerivative (Const _) _                = 0
  secondDerivative (Linear  xs ys) x          = Derivative $ GSL.evaluateDerivative2V  GSL.Linear xs ys (coerce x)
  secondDerivative interp@(LogInterp f) x     = Derivative $ dx * dfx + coerce finterp * d2fdx2
                                                 where (Derivative dfx) =  derivative f x:: Derivative x Double
                                                       (Derivative dx) = derivative interp x :: Derivative x y
                                                       (Derivative d2fdx2) = secondDerivative f x :: Derivative x Double
                                                       finterp = interpolate interp x::y
  secondDerivative (CubicSpline  xs ys) x     = Derivative $ coerce $ GSL.evaluateDerivative2V  GSL.CSpline  xs ys (coerce x)
  secondDerivative (AkimaSpline  xs ys) x     = Derivative $ coerce $ GSL.evaluateDerivative2V  GSL.Akima xs ys (coerce x)
  secondDerivative (PieceWiseConstant _ _ ) _ = 0

lastElement :: SortedList.SortedList (a, b) -> (a, b)
lastElement xys = case SortedList.uncons (SortedList.reverse xys) of
                    Nothing                 -> error "Interpolation is empty"
                    (Just (Down (x, y), _)) -> (x, y)

firstElement :: SortedList.SortedList (a, b) -> (a, b)
firstElement xys = case SortedList.uncons xys of
                     Nothing            -> error "Interpolation is empty"
                     (Just ((x, y), _)) -> (x, y)
