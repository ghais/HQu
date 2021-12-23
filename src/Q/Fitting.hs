{-# LANGUAGE AllowAmbiguousTypes #-}
module Q.Fitting
  where



newtype Model r s = Model {unMoldel :: [r] -> [s]}

class Fittable f r s where
  fittableNumParams    :: f -> Int
  fittableModel        :: f -> Model r s
  fittableSample       :: f -> [s]
  fittableIniitalGuess :: f -> [r]
  fittableBounds       :: f -> [(r, r)]
