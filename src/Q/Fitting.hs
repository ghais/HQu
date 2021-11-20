{-# LANGUAGE AllowAmbiguousTypes #-}
module Q.Fitting
  where



newtype Model r s = Model {unmodel :: [r] -> [s]}

class Fittable f r s where
  fittableNumParams :: f -> Int
  fittableModel :: f -> [r] -> [s]
