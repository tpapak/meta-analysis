{-|
Module      : Data.Meta.Studies
Description : Study definitions
Copyright   : (c) Thodoris Papakonstantinou, 2018
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Study definitions, import, manipulations
-}

{-# LANGUAGE DeriveGeneric       #-}

module Data.Meta.Studies
  ( StudyId (..)
  , Study (..)
  , IVStudy (..)
  , PairwiseStudy (..)
  , StudyGraph (..)
  , getStudyId
  , getStudyArms
  , pairwiseStudyToComparison
  , pairwiseToStudy
  , studiesGraph'
  , studyToIVStudy
  , getEffectsOfIVStudy
  ) where

import           Control.Applicative
import           Data.List.Split
import           Data.Tuple.Extra
import qualified Data.Map.Strict     as Map
import qualified Data.Set as Set
import           Data.Maybe
import Data.List
import qualified Data.IntMap.Strict as IM
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import           GHC.Generics
import Data.Aeson.Text (encodeToLazyText) 
import Data.Graph.AdjacencyList
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
import qualified Data.Csv            as C

import Data.Meta.Effects
import Data.Meta.Multiarm

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

-- | Study as a collection of treatments (arms). This definitions coveres
-- multiarm studies
data Study = BinaryStudy StudyId [Arm] 
           | ContinuousStudy StudyId [Arm]
  deriving (Show, Read, Ord, Eq, Generic)
instance ToJSON Study
instance FromJSON Study

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

-- | Data structure for representing whole network of studies
data Effect a => StudyGraph a = 
  StudyGraph { directGraph :: Graph
             , vsts :: IM.IntMap TreatmentId
             , tsvs :: Map.Map TreatmentId Vertex
             , studies :: Map.Map ComparisonId [(StudyId, a)]
             , directEffects :: Map.Map ComparisonId a -- ^ summary of studies for two stage nma
             }
  deriving (Show, Eq)


getStudyId :: Study -> StudyId
getStudyId (BinaryStudy sid _) = sid
getStudyId (ContinuousStudy sid _) = sid

getStudyArms :: Study -> [Arm]
getStudyArms (BinaryStudy sid arms) = arms
getStudyArms (ContinuousStudy sid arms) = arms

-- | Study as inverse variance estimates
data Effect a => IVStudy a = IVStudy StudyId (Contrasts a)
  deriving (Show, Read, Ord, Eq)

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

-- | Convert list of studies into their network
studiesGraph' :: Effect e => [IVStudy e] 
              -> ([e] -> e)
              -> StudyGraph e
studiesGraph' studyList summarizePairwise =
  let contrasts = concat 
        $ map (\(IVStudy sid cnts) -> 
          let cls = contrastsToList cnts
           in map (\contrast -> (sid, contrast) ) cls) studyList
      studiesEffects = foldl' (\ac (sid, Contrast ta tb e) -> 
                let cid = ComparisonId ta tb
                 in Map.insertWith (\[new] old -> 
                     if Data.List.null old then [new]
                                           else new : old
                           ) cid [(sid, e)] ac) Map.empty contrasts
      treatments = foldl' (\ac (ComparisonId a b)
          -> Set.insert a (Set.insert b ac)) Set.empty $ Map.keys studiesEffects
      vsts = IM.fromList $ zip [1..] (Set.toList treatments)
      tsvs = Map.fromList $ zip (Set.toList treatments) [1..]
      es = let comparisons = Map.keys studiesEffects
            in map (\(ComparisonId a b) ->
              let va = fromJust $ Map.lookup a tsvs 
                  vb = fromJust $ Map.lookup b tsvs
              in Edge va vb) comparisons
      gr = graphFromEdges es
      directEffects = Map.map (\sts -> summarizePairwise (map (\(sid, e) -> e) sts)) studiesEffects
   in StudyGraph { directGraph = gr
                 , vsts = vsts
                 , tsvs = tsvs
                 , studies = studiesEffects
                 , directEffects = directEffects
                 }

getEffectsOfIVStudy :: Effect a => IVStudy a -> [a]
getEffectsOfIVStudy (IVStudy st contrs) =
  let cts = contrastsToList contrs
   in map (\(Contrast ta tb ef) -> ef) cts

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
                let acc' = Map.insert (ComparisonId ta tb) ef acc
                 in Map.insert (ComparisonId tb ta) (reverseEffect ef) acc') Map.empty contrasts
           in Right $ IVStudy sid (Contrasts cntrs)

