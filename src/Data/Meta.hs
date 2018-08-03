{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Meta
  ( TreatmentId (..)
  , Study (..)
  , meanDifference
  ) where

import           Control.Applicative
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           GHC.Generics
import qualified Data.Csv             as C
import Data.Either

data TreatmentId = IntId Int
                 | StringId String
  deriving (Generic,Read,Ord,Eq)
instance Show TreatmentId where
  show (IntId tid)    = show tid
  show (StringId tid) = tid

data Study = 
-- |Constructor for continuous outcomes 
-- |ID, mean, standard deviation, sample size of comparison
      ContinuousStudy { study :: !String 
                      , meanA :: !Double
                      , sdA :: !Double 
                      , nA :: !Int
                      , meanB :: !Double
                      , sdB :: !Double 
                      , nB :: !Int
                      }
-- |Constructor for binary outcomes 
-- |ID, events and number of participants
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
             -- |Variance of the mean (value)
             , variance :: !Double
             }
  deriving (Generic,Read,Ord,Eq,Show)

standardError :: EffectSize -> Double
standardError es = sqrt $ variance es

meanDifference :: Study -> Either String EffectSize
meanDifference (BinaryStudy _ _ _ _ _) = 
 Left "Binary outcome not continuous"
meanDifference s = 
  let ef = x1 - x2
      var = sd1^2 / n1 + sd2^2 / n2
   in Right $ EffectSize { effect = ef
                    -- |Not assuming σ1 = σ2
                         , variance = var
                         }
 where x1 = meanA s
       sd1 = sdA s
       n1 = fromIntegral $ nA s
       x2 = meanB s
       sd2 = sdB s
       n2 = fromIntegral $ nB s


