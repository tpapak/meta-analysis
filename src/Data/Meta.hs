{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Meta
  ( TreatmentId (..)
  , Study (..)
  , EffectSize (..)
  , meanDifference
  , standardizedMeanDifference
  ) where

import           Control.Applicative
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           GHC.Generics
import qualified Data.Csv             as C
import Data.Either

import Data.Numerics

data TreatmentId = IntId Int
                 | StringId String
  deriving (Generic,Read,Ord,Eq)
instance Show TreatmentId where
  show (IntId tid)    = show tid
  show (StringId tid) = tid

data Study = 
      -- |Constructor for continuous outcomes 
      --  ID, mean, standard deviation, sample size of comparison
      ContinuousStudy { study :: !String 
                      , meanA :: !Double
                      , sdA :: !Double 
                      , nA :: !Int
                      , meanB :: !Double
                      , sdB :: !Double 
                      , nB :: !Int
                      }
      -- |Constructor for binary outcomes 
      --  ID, events and number of participants
      | BinaryStudy { study :: !String 
                    , eventsA :: !Int
                    , nA :: !Int
                    , eventsB :: !Int
                    , nB :: !Int
                    }
  deriving (Generic,Read,Ord,Eq,Show)
instance C.FromRecord Study
instance C.FromNamedRecord Study
instance C.ToNamedRecord Study

data EffectSize =
  EffectSize { effect :: !Double
             -- |Variance of the __mean__
             , variance :: !Double
             }
  deriving (Generic,Read,Ord,Eq,Show)

standardError :: EffectSize -> Double
standardError es = sqrt $ variance es

meanDifference :: Study -> Either String EffectSize
meanDifference (BinaryStudy _ _ _ _ _) = 
  Left "Binary outcome not continuous"
-- |Not assuming σ1 = σ2 (4.5)
meanDifference s = Right $ 
  EffectSize { effect = x1 - x2
             , variance = sd1^2 / n1 + sd2^2 / n2 
             }
 where x1 = meanA s
       sd1 = sdA s
       n1 = fromIntegral $ nA s
       x2 = meanB s
       sd2 = sdB s
       n2 = fromIntegral $ nB s

-- | Applied Hedges' correction
standardizedMeanDifference :: Study -> Either String EffectSize
standardizedMeanDifference (BinaryStudy _ _ _ _ _) =
  Left "Binary outcome not continuous"
standardizedMeanDifference (ContinuousStudy stid x1 s1 na x2 s2 nb) =
  let swithin = sqrt $ ((n1 - 1) * s1^2 + (n2 -1) * s2^2) / (n1 + n2 - 2) -- (4.19)
      d = (x1 - x2) / swithin -- (4.18)
      vd = (n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)) -- (4.20)
      dof = n1 + n2 - 2
      j = 1 - (3 / (4 * dof - 1)) --  (4.22)
      g = j * d -- (4.23)
      vg = (j^2) * vd -- (4.24)
  in Right $ EffectSize { effect = g
                        , variance = vg
                        }
  where n1 = fromIntegral na
        n2 = fromIntegral nb
