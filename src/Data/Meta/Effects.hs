{-|
Module      : Meta.Effects
Description : Effect size calculations 
Copyright   : (c) Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Effect size calculations for binary and countinuous outcomes
following Borenstein et al's Introduction to Meta-Analysis
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Meta.Effects
  ( TreatmentId (..)
  , Study (..)
  , PairwiseStudy (..)
  , PointEstimate (..)
  , Variance (..)
  -- * Effect class
  , Estimate (..)
  -- * Gaussian estimate class
  , Gaussian (..)
  -- ** Continuous
  , MD (..)
  , SMD (..)
  -- ** Binary
  , LogRR (..)
  , LogOR (..)
  , OR (..)
  , RR (..)
  , RD (..)
  , ConfidenceInterval (..)
  , ComparisonId (..)
  , pairwiseStudyToArms
  , normalCI
  , ciToVariance
  , invcumul975
  , meanDifference
  , standardizedMeanDifference
  , logRiskRatio
  , riskRatio
  , logOddsRatio
  , oddsRatio
  , riskDifference
  , logORToOR
  , orToLogOR
  , logRRToRR
  , rrToLogRR
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           GHC.Generics
import           Data.List.Split
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
import qualified Data.Csv             as C
import Data.Either

import Data.Numerics

data TreatmentId = IntId Int
                 | StringId String
  deriving (Generic,Read,Ord,Eq)
instance Show TreatmentId where
  show (IntId tid)    = show tid
  show (StringId tid) = tid
instance ToJSON TreatmentId
instance FromJSON TreatmentId
  where
    parseJSON = do
      let outint = withScientific "TreatmentId"
                   $ \tid -> return (IntId (floor tid))
          outstr = withText "TreatmentId"
                   $ \tid -> return (StringId (T.unpack tid))
       in (\v -> outint v <|> outstr v)

data StudyId = StIntId Int
             | StStringId String
  deriving (Generic,Read,Ord,Eq)
instance Show StudyId where
  show (StIntId tid)    = show tid
  show (StStringId tid) = tid
instance ToJSON StudyId
instance FromJSON StudyId
  where
    parseJSON = do
      let outint = withScientific "StudyId"
                   $ \tid -> return (StIntId (floor tid))
          outstr = withText "StudyId"
                   $ \tid -> return (StStringId (T.unpack tid))
       in (\v -> outint v <|> outstr v)

-- | Easy Study definition for csv reading pairwise studies
data PairwiseStudy = 
      -- |Constructor for continuous outcomes 
      --  ID, mean, standard deviation, sample size of comparison
      CSVContinuousStudy { study :: !String 
                         , meanA :: !Double
                         , sdA :: !Double 
                         , nA :: !Int
                         , meanB :: !Double
                         , sdB :: !Double 
                         , nB :: !Int
                         }
      -- |Constructor for binary outcomes 
      --  ID, events and number of participants
      | CSVBinaryStudy { study :: !String 
                       , eventsA :: !Int
                       , nA :: !Int
                       , eventsB :: !Int
                       , nB :: !Int
                       }
  deriving (Generic,Read,Ord,Eq,Show)
instance C.FromRecord PairwiseStudy
instance C.FromNamedRecord PairwiseStudy
instance C.ToNamedRecord PairwiseStudy

data ComparisonId = ComparisonId TreatmentId TreatmentId
  deriving (Generic,Eq,Ord)
instance Show ComparisonId where
  show (ComparisonId a b) =
     show a ++ ":" ++ show b
instance ToJSON ComparisonId
instance FromJSON ComparisonId
  where
    parseJSON = do
      let compstr = withText "ComparisonId"
                   $ \cid -> do
                     let textToTid tx =
                           let etx = TR.decimal (T.pack tx)
                            in case etx of
                                 Left ert -> StringId tx
                                 Right (nid,rst) -> case (T.unpack rst) of
                                                      "" -> IntId nid
                                                      _  -> StringId tx
                         comps = splitOn ":" (T.unpack cid)
                      in return $ ComparisonId (textToTid (head comps)) (textToTid (last comps))
       in (\v -> compstr v)

this :: ComparisonId -> TreatmentId
this (ComparisonId a b) = a

that :: ComparisonId -> TreatmentId
that (ComparisonId a b) = b

type Events = Int
type SampleSize = Int
type MeanEffect = Double
type SDEffect = Double

data Arm = BinaryArm TreatmentId Events SampleSize  
         | ContinuousArm TreatmentId MeanEffect SDEffect SampleSize
  deriving (Show, Generic, Ord, Eq)

data Estimate a => Contrast a = 
  BinaryContrast TreatmentId TreatmentId a |
  ContinuousContrast TreatmentId TreatmentId a
  deriving (Show, Ord, Eq)

-- | Long format (one row per Arm) or one row per contrast (inverse variance format)
data Study = BinaryStudy StudyId [Arm] 
           | ContinuousStudy StudyId [Arm]

type PointEstimate = Double
type Variance = Double
type StandardError = Double
data ConfidenceInterval = CI { lower :: Double
                             , upper :: Double
                             }
  deriving (Generic,Read,Ord,Eq,Show)

invcumul975 = 1.959964

-- | get the confidence interval of an effect given its variance
normalCI :: Gaussian g => g -> ConfidenceInterval
normalCI d =
  let μ = expectation d  
      v = variance d
      se = sqrt v
   in CI (μ - invcumul975 * se) (μ + invcumul975 * se)

ciToVariance :: ConfidenceInterval -> Variance
ciToVariance (CI l u) =
  let sd = (u - l) / (2 * invcumul975)
   in sd^2

class Estimate e where
  point :: e -> Double -- ^ the point estimate
  ci :: e -> ConfidenceInterval -- ^ lower upper values of 95% confidence interval
  mapEstimate :: (Double -> Double) -> e -> e

class Gaussian e where
  expectation :: e -> Double
  variance :: e -> Double

data MD = MD Double Double -- ^ Mean difference
  deriving (Read,Ord,Eq,Show)
instance Estimate MD where
  point (MD p v) = p
  ci = normalCI
  mapEstimate f (MD p v) = MD (f p) (f v)
instance Gaussian MD where
  expectation (MD p v) = p
  variance (MD p v) = v

data SMD = SMD PointEstimate Variance -- ^ Standardized Mean Difference
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate SMD where
  point (SMD p v) = p
  ci = normalCI
  mapEstimate f (SMD p v) = SMD (f p) (f v)
instance Gaussian SMD where
  expectation (SMD p v) = p
  variance (SMD p v) = v

data LogOR = LogOR PointEstimate Variance -- ^ Log Odds Ratio
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate LogOR where
  point (LogOR p v) = p
  ci = normalCI
  mapEstimate f (LogOR p v) = LogOR (f p) (f v)
instance Gaussian LogOR where
  expectation (LogOR p v) = p
  variance (LogOR p v) = v

data LogRR = LogRR PointEstimate Variance -- ^ Log Risk Ratio
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate LogRR where
  point (LogRR p v) = p
  ci = normalCI
  mapEstimate f (LogRR p v) = LogRR (f p) (f v)
instance Gaussian LogRR where
  expectation (LogRR p v) = p
  variance (LogRR p v) = v

data OR = OR PointEstimate ConfidenceInterval -- ^ Odds Ratio
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate OR where
  point (OR p ci) = p
  ci (OR p ci) = ci
  mapEstimate f (OR p ci) = 
    let nl = f $ lower ci
        nu = f $ upper ci
     in OR (f p) (CI nl nu)

data RR = RR PointEstimate ConfidenceInterval -- ^ Risk Ratio
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate RR where
  point (RR p ci) = p
  ci (RR p ci) = ci
  mapEstimate f (RR p ci) = 
    let nl = f $ lower ci
        nu = f $ upper ci
     in RR (f p) (CI nl nu)

data RD = RD PointEstimate Variance -- ^ Risk Difference
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate RD where
  point (RD p v) = p
  ci = normalCI
  mapEstimate f (RD p v) = RD (f p) (f v)
instance Gaussian RD where
  expectation (RD p v) = p
  variance (RD p v) = v

-- | checks if CI is messed up
checkCI :: ConfidenceInterval -> Bool
checkCI (CI l u) = l < u

pairwiseStudyToArms :: PairwiseStudy -> (Arm, Arm)
pairwiseStudyToArms (CSVBinaryStudy sid ea na eb nb)
  = ( BinaryArm (StringId "A") ea na
    , BinaryArm (StringId "B") eb nb
    )
pairwiseStudyToArms s
  = ( ContinuousArm (StringId "A") (meanA s) (sdA s) (nA s)
    , ContinuousArm (StringId "B") (meanB s) (sdB s) (nB s)
    )

notCompatibleArms :: (Arm, Arm) -> Bool
notCompatibleArms ((BinaryArm _ _ _), (BinaryArm _ _ _)) = True
notCompatibleArms ((ContinuousArm _ _ _ _), (ContinuousArm _ _ _ _)) = True
notCompatibleArms _ = False

meanDifference :: (Arm, Arm) -> Either String MD
meanDifference ((BinaryArm _ _ _), _) = 
  Left "Binary outcome not continuous"
meanDifference (_, (BinaryArm _ _ _)) = 
  Left "Binary outcome not continuous"
-- |Not assuming σ1 = σ2 (4.5)
meanDifference ( (ContinuousArm tid1 x1 sd1 n1) 
               , (ContinuousArm tid2 x2 sd2 n2) )
    = Right $ MD (x1 - x2) (sd1^2 / n1' + sd2^2 / n2')
 where n1' = fromIntegral $ n1
       n2' = fromIntegral $ n2

-- | Applied Hedges' correction
standardizedMeanDifference :: (Arm, Arm)-> Either String SMD
standardizedMeanDifference ((BinaryArm _ _ _), _) = 
  Left "Binary outcome not continuous"
standardizedMeanDifference (_, (BinaryArm _ _ _)) = 
  Left "Binary outcome not continuous"
standardizedMeanDifference ( (ContinuousArm tid1 x1 s1 na) 
                           , (ContinuousArm tid2 x2 s2 nb) ) =
  let swithin = sqrt $ ((n1 - 1) * s1^2 + (n2 -1) * s2^2) / (n1 + n2 - 2) -- (4.19)
      d = (x1 - x2) / swithin -- (4.18)
      vd = (n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)) -- (4.20)
      dof = n1 + n2 - 2
      j = 1 - (3 / (4 * dof - 1)) -- (4.22)
      g = j * d -- (4.23)
      vg = (j^2) * vd -- (4.24)
   in Right $ SMD g vg
  where n1 = fromIntegral na
        n2 = fromIntegral nb


logRiskRatio :: (Arm, Arm) -> Either String LogRR
logRiskRatio ((ContinuousArm _ _ _ _), _) = 
  Left "Outcome not Binary"
logRiskRatio (_, (ContinuousArm _ _ _ _)) = 
  Left "Outcome not Binary"
logRiskRatio ( (BinaryArm _ ea na) 
             , (BinaryArm _ eb nb) ) =
  let rr = (a/n1) / (c/n2) -- (5.1)
      logrr = log rr -- (5.2)
      var = 1/a - 1/n1 + 1/c - 1/n2 -- (5.3)
   in Right $ LogRR logrr var
  where a = fromIntegral ea
        c = fromIntegral eb
        n1 = fromIntegral na
        n2 = fromIntegral nb

riskRatio :: (Arm, Arm) -> Either String RR
riskRatio ((ContinuousArm _ _ _ _), _) = 
  Left "Outcome not Binary"
riskRatio (_, (ContinuousArm _ _ _ _)) = 
  Left "Outcome not Binary"
riskRatio ( (BinaryArm t1 ea na) 
          , (BinaryArm t2 eb nb) ) =
  let comparison = ( (BinaryArm t1 ea na) 
                   , (BinaryArm t2 eb nb) )
      elnRR = logRiskRatio comparison
      rr = (a/n1) / (c/n2) -- (5.1)
   in case elnRR of 
         Left err -> Left err
         Right lnRR ->
           let lnRRCI = ci lnRR
               rrci = CI ((exp . lower) lnRRCI) ((exp . upper) lnRRCI)
            in Right $ RR rr rrci
  where a = fromIntegral ea
        c = fromIntegral eb
        n1 = fromIntegral na
        n2 = fromIntegral nb

logOddsRatio :: (Arm, Arm) -> Either String LogOR
logOddsRatio ((ContinuousArm _ _ _ _), _) = 
  Left "Outcome not Binary"
logOddsRatio (_, (ContinuousArm _ _ _ _)) = 
  Left "Outcome not Binary"
logOddsRatio ( (BinaryArm _ ea na) 
             , (BinaryArm _ eb nb) ) = 
  let or = (a*d) / (b*c) -- (5.8)
      logor = log or -- (5.9)
      var = 1/a + 1/b + 1/c + 1/d -- (5.10)
   in Right $ LogOR logor var
  where a = fromIntegral ea
        b = n1 - a
        c = fromIntegral eb
        d = n2 - c
        n1 = fromIntegral na
        n2 = fromIntegral nb

oddsRatio :: (Arm, Arm) -> Either String OR
oddsRatio ((ContinuousArm _ _ _ _), _) = 
  Left "Outcome not Binary"
oddsRatio (_, (ContinuousArm _ _ _ _)) = 
  Left "Outcome not Binary"
oddsRatio ( (BinaryArm t1 ea na) 
          , (BinaryArm t2 eb nb) ) =
  let or = (a*d) / (b*c) -- (5.8)
      comparison = ( (BinaryArm t1 ea na) 
                   , (BinaryArm t2 eb nb) )
      elnOR = logOddsRatio comparison
   in case elnOR of 
         Left err -> Left err
         Right lnOR ->
           let lnORCI = ci lnOR
               orci = CI ((exp . lower) lnORCI) ((exp . upper) lnORCI)
            in Right $ OR or orci
  where a = fromIntegral ea
        b = n1 - a
        c = fromIntegral eb
        d = n2 - c
        n1 = fromIntegral na
        n2 = fromIntegral nb

logORToOR :: LogOR -> OR
logORToOR e = 
  let p = exp $ point e
      lnORCI = ci e
      orci = CI ((exp . lower) lnORCI) ((exp . upper) lnORCI)
   in OR p orci

orToLogOR :: OR -> LogOR
orToLogOR (OR e (CI l u)) = 
  let pe = log e
      ci = CI (log l) (log u)
   in LogOR pe (ciToVariance ci)

logRRToRR :: LogRR -> RR
logRRToRR e = 
  let p = exp $ point e
      lnRRCI = ci e
      rrci = CI ((exp . lower) lnRRCI) ((exp . upper) lnRRCI)
   in RR p rrci

rrToLogRR :: RR -> LogRR
rrToLogRR (RR e (CI l u)) = 
  let pe = log e
      ci = CI (log l) (log u)
   in LogRR pe (ciToVariance ci)

riskDifference :: (Arm, Arm) -> Either String RD
riskDirfference ((ContinuousArm _ _ _ _), _) = 
  Left "Outcome not Binary"
riskDifference (_, (ContinuousArm _ _ _ _)) = 
  Left "Outcome not Binary"
riskDifference ( (BinaryArm t1 ea na) 
               , (BinaryArm t2 eb nb) ) =
  let rd = (a/n1) - (c/n2) -- (5.15)
      var = (a * b) / n1^3 + (c * d) / n2^3 -- (5.16)
   in Right $ RD rd var
  where a = fromIntegral ea
        b = n1 - a
        c = fromIntegral eb
        d = n2 - c
        n1 = fromIntegral na
        n2 = fromIntegral nb
