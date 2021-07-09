{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Data.Random.Distribution.MultivariateNormal
-- Copyright   :  (c) 2016 FP Complete Corporation
-- License     :  MIT (see LICENSE)
-- Maintainer  :  dominic@steinitz.org
module Q.Util.File (write)
  where

import Numeric.LinearAlgebra
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Csv
import Data.Char (ord)
import qualified Data.ByteString.Lazy as B
import Data.Random


rowToRecord :: (Show t) => [t] -> Record
rowToRecord x = record $ map (C.pack . show) x

write :: (Show t) => [[t]] -> [String] -> FilePath -> IO ()
write m header path = do
  let out = (encodeWith opt s) where
        opt = defaultEncodeOptions { encDelimiter = fromIntegral (ord ','), encQuoting = QuoteNone }
        rows :: [Record]
        rows = map rowToRecord $ m
        header_ = record $ map C.pack  header
        s = if null header_ then rows else header_:rows
  B.writeFile path out


