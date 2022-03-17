module Data.Numerics
  ( Roundable (..)
  , roundDouble
  ) where

roundDouble :: Double -> Int -> Double
roundDouble f n =
     (fromInteger $ round $ f * (10^n)) / (10.0^^n)


{-roundEffect :: Effect a => a -> a-}
{-roundEffect e =-}
  {-let -}

class Roundable r where
  roundme :: r -> Int -> r
