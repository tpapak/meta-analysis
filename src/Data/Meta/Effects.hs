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
  ( StringIntId (..)
  , TreatmentId (..)
  , StudyId (..)
  , Study (..)
  , IVStudy (..)
  , PairwiseStudy (..)
  , PointEstimate (..)
  , Variance (..)
  -- * Effect class
  , Estimate (..)
  , Effect (..)
  , Linear (..)
  , Contrast (..)
  , Arm (..)
  -- * Just a pair of arms
  , Comparison (..)
  , ComparisonId (..)
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
  , tidOfArm
  , getStudyId
  , getStudyArms
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
  , pairwiseStudyToComparison
  , pairwiseToStudy
  , armsToComparisons
  , comparisonToContrast
  , contrastsToList
  , studyToIVStudy
  , getEffectsOfIVStudy
  ) where

import           Control.Applicative
import           Data.Tuple.Extra
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
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText) 
import Data.Numerics

data StringIntId = IntId Int
                 | StringId String
  deriving (Generic, Read, Ord, Eq)
instance ToJSON StringIntId
instance Show StringIntId where
  show (IntId tid)    = show tid
  show (StringId tid) = tid
instance C.FromRecord StringIntId
instance C.FromNamedRecord StringIntId
instance C.ToNamedRecord StringIntId

newtype TreatmentId = TreatmentId StringIntId 
  deriving (Generic, Show, Read, Ord, Eq)
instance ToJSON TreatmentId
instance FromJSON TreatmentId
  where
    parseJSON = do
      let outint = withScientific "TreatmentId"
                    $ \tid -> return (TreatmentId $ IntId (floor tid))
          outstr = withText "TreatmentId"
                    $ \tid -> return (TreatmentId $ StringId (T.unpack tid))
       in (\v -> outint v <|> outstr v)

newtype StudyId = StudyId StringIntId 
  deriving (Generic, Show, Read,Ord,Eq)
instance ToJSON StudyId
instance FromJSON StudyId
  where
    parseJSON = do
      let outint = withScientific "StudyId"
                    $ \tid -> return (StudyId $ IntId (floor tid))
          outstr = withText "StudyId"
                    $ \tid -> return (StudyId $ StringId (T.unpack tid))
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
                                 Left ert -> TreatmentId $ StringId tx
                                 Right (nid,rst) -> case (T.unpack rst) of
                                                      "" -> TreatmentId $ IntId nid
                                                      _  -> TreatmentId $ StringId tx
                         comps = splitOn ":" (T.unpack cid)
                      in return $ ComparisonId (textToTid (head comps)) (textToTid (last comps))
       in (\v -> compstr v)

this :: ComparisonId -> TreatmentId
this (ComparisonId a b) = a

that :: ComparisonId -> TreatmentId
that (ComparisonId a b) = b

type Comparison = (Arm, Arm)

type Events = Int
type SampleSize = Int
type MeanEffect = Double
type SDEffect = Double

-- | Arm definition for binary and continuous data
data Arm = BinaryArm TreatmentId Events SampleSize  
         | ContinuousArm TreatmentId MeanEffect SDEffect SampleSize
  deriving (Show, Generic, Read, Ord)
instance Eq Arm
    where arm1 == arm2 = tidOfArm arm1 == tidOfArm arm2
instance ToJSON Arm
instance FromJSON Arm

-- | Treatment vs Treatment
data Effect a => Contrast a = Contrast TreatmentId TreatmentId a
  deriving (Show, Read, Ord, Eq)

data Effect a => Contrasts a 
  = Contrasts (Map.Map TreatmentId (Map.Map TreatmentId a))
  deriving (Show, Read, Ord, Eq)

-- | Study as a collection of treatments (arms). This definitions coveres
-- multiarm studies
data Study = BinaryStudy StudyId [Arm] 
           | ContinuousStudy StudyId [Arm]
  deriving (Show, Read, Ord, Eq, Generic)
instance ToJSON Study
instance FromJSON Study

getStudyId :: Study -> StudyId
getStudyId (BinaryStudy sid _) = sid
getStudyId (ContinuousStudy sid _) = sid

getStudyArms :: Study -> [Arm]
getStudyArms (BinaryStudy sid arms) = arms
getStudyArms (ContinuousStudy sid arms) = arms

-- | Study as inverse variance estimates
data Effect a => IVStudy a = IVStudy StudyId (Contrasts a)
  deriving (Show, Read, Ord, Eq)

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

-- | Class for propabilistic values with point estimates and uncertainty
class Estimate e where
  point :: e -> Double -- ^ the point estimate
  ci :: e -> ConfidenceInterval -- ^ lower upper values of 95% confidence interval
  mapEstimate :: (Double -> Double) -> e -> e

class Estimate e => Effect e where
  isBinary :: e -> Bool -- ^ true if binary outcome false otherwise

class (Effect e , Estimate e) => Linear e where
  null :: e -> Double
  translate :: e -> Double -> e
  negate :: e -> e

class Gaussian e where
  expectation :: e -> Double
  variance :: e -> Double

data MD = MD Double Double -- ^ Mean difference
  deriving (Read,Ord,Eq,Show, Generic)
instance Estimate MD where
  point (MD p v) = p
  ci = normalCI
  mapEstimate f (MD p v) = MD (f p) (f v)
instance Effect MD where
  isBinary _ = False
instance Linear MD where
  null _ = 0
  translate (MD p v) x = MD (p+x) v
  negate (MD p v) = MD (-p) v
instance Gaussian MD where
  expectation (MD p v) = p
  variance (MD p v) = v

data SMD = SMD PointEstimate Variance -- ^ Standardized Mean Difference
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate SMD where
  point (SMD p v) = p
  ci = normalCI
  mapEstimate f (SMD p v) = SMD (f p) (f v)
instance Effect SMD where
  isBinary _ = False
instance Linear SMD where
  null _ = 0
  translate (SMD p v) x = SMD (p+x) v
  negate (SMD p v) = SMD (-p) v
instance Gaussian SMD where
  expectation (SMD p v) = p
  variance (SMD p v) = v

data LogOR = LogOR PointEstimate Variance -- ^ Log Odds Ratio
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate LogOR where
  point (LogOR p v) = p
  ci = normalCI
  mapEstimate f (LogOR p v) = LogOR (f p) (f v)
instance Effect LogOR where
  isBinary _ = True
instance Linear LogOR where
  null _ = 0
  translate (LogOR p v) x = LogOR (p+x) v
  negate (LogOR p v) = LogOR (-p) v
instance Gaussian LogOR where
  expectation (LogOR p v) = p
  variance (LogOR p v) = v

data LogRR = LogRR PointEstimate Variance -- ^ Log Risk Ratio
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate LogRR where
  point (LogRR p v) = p
  ci = normalCI
  mapEstimate f (LogRR p v) = LogRR (f p) (f v)
instance Effect LogRR where
  isBinary _ = True
instance Linear LogRR where
  null _ = 0
  translate (LogRR p v) x = LogRR (p+x) v
  negate (LogRR p v) = LogRR (-p) v
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
instance Effect OR where
  isBinary _ = True

data RR = RR PointEstimate ConfidenceInterval -- ^ Risk Ratio
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate RR where
  point (RR p ci) = p
  ci (RR p ci) = ci
  mapEstimate f (RR p ci) = 
    let nl = f $ lower ci
        nu = f $ upper ci
     in RR (f p) (CI nl nu)
instance Effect RR where
  isBinary _ = True

data RD = RD PointEstimate Variance -- ^ Risk Difference
  deriving (Generic,Read,Ord,Eq,Show)
instance Estimate RD where
  point (RD p v) = p
  ci = normalCI
  mapEstimate f (RD p v) = RD (f p) (f v)
instance Effect RD where
  isBinary _ = True
instance Linear RD where
  null _ = 0
  translate (RD p v) x = RD (p+x) v
  negate (RD p v) = RD (-p) v
instance Gaussian RD where
  expectation (RD p v) = p
  variance (RD p v) = v

-- | checks if CI is messed up
checkCI :: ConfidenceInterval -> Bool
checkCI (CI l u) = l < u

pairwiseStudyToComparison :: PairwiseStudy -> Comparison
pairwiseStudyToComparison (CSVBinaryStudy sid ea na eb nb)
  = ( BinaryArm (TreatmentId $ StringId "A") ea na
    , BinaryArm (TreatmentId $ StringId "B") eb nb
    )
pairwiseStudyToComparison s
  = ( ContinuousArm (TreatmentId $ StringId "A") (meanA s) (sdA s) (nA s)
    , ContinuousArm (TreatmentId $ StringId "B") (meanB s) (sdB s) (nB s)
    )

pairwiseToStudy :: PairwiseStudy -> Study
pairwiseToStudy (CSVContinuousStudy sid ma sa na mb sb nb) = 
  let pwst = (CSVContinuousStudy sid ma sa na mb sb nb) 
      comparison = pairwiseStudyToComparison pwst
   in ContinuousStudy (StudyId $ StringId sid) [fst comparison, snd comparison]
pairwiseToStudy (CSVBinaryStudy sid ea na eb nb) = 
  let pwst = (CSVBinaryStudy sid ea na eb nb)
      comparison = pairwiseStudyToComparison pwst
   in BinaryStudy (StudyId $ StringId sid) [fst comparison, snd comparison]

tidOfArm :: Arm -> TreatmentId
tidOfArm (ContinuousArm tidA _ _ _) = tidA
tidOfArm (BinaryArm tidA _ _) = tidA

comparisonToContrast :: Effect a => (Comparison -> Either String a) 
               -> Comparison 
               -> Either String (Contrast a)
comparisonToContrast getEffect comparison = 
  let eeffect = getEffect comparison
      (tidA, tidB) = both tidOfArm comparison
   in case eeffect of 
        Left err -> Left err
        Right effect -> Right $ Contrast tidA tidB effect

armsToComparisons :: [Arm] -> [Comparison]
armsToComparisons arms = 
  let allcomps = (,) <$> arms <*> arms
   in filter (\(a,b) -> a/=b && a < b) allcomps

studyToIVStudy :: Effect a => Study 
                -> (Comparison -> Either String a) 
                -> Either String (IVStudy a)
studyToIVStudy st getEffect =
  let sid = getStudyId st
      arms = getStudyArms st
      eef = sequence $ map (comparisonToContrast getEffect) $ armsToComparisons arms
   in case eef of
        Left err -> Left err
        Right contrasts -> 
          let cntrs = foldl (\acc (Contrast ta tb ef) -> 
                      Map.insert ta (Map.singleton tb ef) acc) Map.empty contrasts
           in Right $ IVStudy sid (Contrasts cntrs)

contrastsToList :: Effect e => Contrasts e -> [Contrast e]
contrastsToList (Contrasts e) = 
  let cts = Map.toList e
   in map (\(ta, tbef) ->
                    let (tb, ef) = head $ Map.toList tbef
                     in Contrast ta tb ef
          ) cts

getEffectsOfIVStudy :: Effect a => IVStudy a -> [a]
getEffectsOfIVStudy (IVStudy st contrs) =
  let cts = contrastsToList contrs
   in map (\(Contrast ta tb ef) -> ef) cts


meanDifference :: Comparison -> Either String MD
-- |Not assuming σ1 = σ2 (4.5)
meanDifference ( (ContinuousArm tid1 x1 sd1 n1) 
               , (ContinuousArm tid2 x2 sd2 n2) )
    = Right $ MD (x1 - x2) (sd1^2 / n1' + sd2^2 / n2')
 where n1' = fromIntegral $ n1
       n2' = fromIntegral $ n2
meanDifference (_, _) = 
  Left "Not continuous outcome"

-- | Applied Hedges' correction
standardizedMeanDifference :: Comparison-> Either String SMD
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


logRiskRatio :: Comparison -> Either String LogRR
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

riskRatio :: Comparison -> Either String RR
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

logOddsRatio :: Comparison -> Either String LogOR
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

oddsRatio :: Comparison -> Either String OR
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

riskDifference :: Comparison -> Either String RD
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
