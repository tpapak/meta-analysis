{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Meta
  ( TreatmentId (..)
  , Study
  ) where

import           Control.Applicative
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           GHC.Generics
import qualified Data.Csv             as C

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
