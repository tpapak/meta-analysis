module Data.Numerics
  ( roundDouble
  ) where

roundDouble :: Double -> Int -> Double
roundDouble f n =
     (fromInteger $ round $ f * (10^n)) / (10.0^^n)
