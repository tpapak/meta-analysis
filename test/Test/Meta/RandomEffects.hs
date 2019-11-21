module Test.Meta.RandomEffects where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy             as B
import qualified Data.Map.Strict                  as Map
import qualified Data.Vector as V
import qualified Data.Csv             as C
import Data.Either

import           TestHS

import Data.Numerics
import           Data.Meta.Effects
import           Data.Meta.Pairwise.RandomEffects

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ smd
          , rr
          , testor
          , testrd
          , testtau
          ]

smd :: IO Test
smd = do
  let name = "Random Effects meta-analysis on continuous SMD"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let emds = rights $ fmap (standardizedMeanDifference . pairwiseStudyToComparison) $ V.toList studies 
      let ce = summary $ randomEffects emds
      let (SMD e v) = ce
      let foundce = (mapEstimate (\c -> roundDouble c 4) (SMD e v))
      let expected = SMD 0.3582 0.0111
      if  foundce == expected
        then
          return $ testPassed name $ "nice!!"
        else
          return $ testFailed name $ (show expected, show foundce)

rr :: IO Test
rr = do
  let name = "Random Effects meta-analysis on binary RR"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let emds = rights $ fmap (riskRatio .pairwiseStudyToComparison) $ V.toList studies 
      let ce = randomEffectsRR emds
      let foundce = (mapEstimate (\c -> roundDouble c 4) ce)
      let expected = RR 0.6395 (CI 0.4283 0.9548)
      if  foundce == expected
        then
          return $ testPassed name $ "nice!!"
        else
          return $ testFailed name $ (show expected, show foundce)

testor :: IO Test
testor = do
  let name = "Random Effects meta-analysis on binary OR"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let emds = rights $ fmap (oddsRatio . pairwiseStudyToComparison) $ V.toList studies 
      let ce = randomEffectsOR emds
      let foundce = (mapEstimate (\c -> roundDouble c 4) ce)
      let expected = OR 0.5676 (CI 0.3554 0.9065)
      if  foundce == expected
        then
          return $ testPassed name $ "nice!!"
        else
          return $ testFailed name $ (show expected, show foundce)

testrd :: IO Test
testrd = do
  let name = "Random Effects meta-analysis on binary RD"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let emds = rights $ fmap (riskDifference . pairwiseStudyToComparison) $ V.toList studies 
      let ce = summary $ randomEffects emds
      let foundce = (mapEstimate (\c -> roundDouble c 4) ce)
      let expected = RD (-0.1119) $ roundDouble (ciToVariance (CI (-0.1499) (-0.0739))) 4
      if  foundce == expected
        then
          return $ testPassed name $ "nice!!"
        else
          return $ testFailed name $ (show expected, show foundce)

testtau :: IO Test
testtau = do
  let name = "Random Effects meta-analysis tau test"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let emds = rights $ fmap (standardizedMeanDifference . pairwiseStudyToComparison)
               $ V.toList studies 
      let ce = randomEffects emds
      let foundce = (mapEstimate (\c -> roundDouble c 6) (τsq ce))
      let expected = TauSquare 0.037311 0.001761 (CI 0 0.131214360151206)
      if  foundce == expected
        then
          return $ testPassed name $ "nice!!"
        else
          return $ testFailed name $ (show expected, show foundce)

