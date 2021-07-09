{-|
Module      : Q.Plotting
Description : A collection of plotting tools i found useful.
-}
{-# LANGUAGE OverloadedStrings          #-}
module Q.Plotting where
import qualified Data.Text              as T

colorPairs :: [(T.Text, T.Text)]
colorPairs = cycle [("#001f3f", "#FF851B"), ("#0074D9", "#FF4136"),("#7FDBFF", "#85144b"), ("#3D9970", "#B10DC9")]
