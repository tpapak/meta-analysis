module Test.Meta.Effects where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           Data.Text.Lazy                 ( Text )
import           Data.Text.Lazy.IO             as I
import           Data.Aeson.Text                ( encodeToLazyText )
import qualified Data.Vector                   as V
import qualified Data.Csv                      as C
import           Data.Either

import           TestHS

import           Data.Numerics
import           Data.Meta.Effects

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests =
  [ test1
  , test2
  , test3
  , test4
  , testLogOR
  , testRR
  , testOR
  , testRD
  , ivstudies
  , reverseOR
  , reverseRR
  , reverseRD
  ]

test1 :: IO Test
test1 = do
  let name        = "read continuous study"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeWith C.defaultDecodeOptions C.HasHeader csvData :: Either
            String
            (V.Vector PairwiseStudy)
  case estudies of
    Left  err     -> return $ testFailed name $ ("error parsing csv", err)
    Right studies -> do
      {-putStrLn "Studies file"-}
      {-putStrLn $ show studies-}
      return $ testPassed name $ "passed!"

test2 :: IO Test
test2 = do
  let name        = "read binary study"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      {-putStrLn "Studies file"-}
      {-putStrLn $ show studies-}
      return $ testPassed name $ "passed!"

test3 :: IO Test
test3 = do
  let name        = "mean differences"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let meandiffs = V.toList
            $ V.map (meanDifference . pairwiseStudyToComparison) studies
      {-putStrLn "mean differences of continuous"-}
      {-putStrLn $ show $ rights meandiffs-}
      return $ testPassed name $ "passed!"

test4 :: IO Test
test4 = do
  let name        = "standardized mean differences"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      {-putStrLn "standardized mean differences of continuous"-}
      let smds = rights $ V.toList $ V.map
            (standardizedMeanDifference . pairwiseStudyToComparison)
            studies
          gs         = map ((\s -> (roundDouble s 3)) . expectation) smds
          vgs        = map ((\s -> (roundDouble s 3)) . variance) smds
          correctgs  = [0.095, 0.277, 0.367, 0.664, 0.462, 0.185]
          correctvgs = [0.033, 0.031, 0.050, 0.011, 0.043, 0.023]
      if gs == correctgs && vgs == correctvgs
        then return $ testPassed name $ "passed!"
        else
          return
          $ testFailed name
          $ ( "wrong swithin values"
            , (show gs <> show correctgs <> show vgs <> show correctvgs)
            )

testLogOR :: IO Test
testLogOR = do
  let name        = "log Odds Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let lnORs = rights $ V.toList $ V.map
            (logOddsRatio . pairwiseStudyToComparison)
            studies
          estimates    = map ((\s -> (roundDouble s 4)) . expectation) lnORs
          variances    = map ((\s -> (roundDouble s 4)) . variance) lnORs
          correctlnORs = [-0.3662, -0.2877, -0.3842, -1.3218, -0.4169, -0.1595]
          correctvars  = [0.1851, 0.2896, 0.1556, 0.0583, 0.2816, 0.1597]
      if estimates == correctlnORs
        then if variances == correctvars
          then return $ testPassed name $ "passed!"
          else
            return
            $ testFailed name
            $ ("wrong lnRRs variances", show variances <> show correctvars)
        else
          return
          $ testFailed name
          $ ("wrong lnRRs", show estimates <> show correctlnORs)

testRR :: IO Test
testRR = do
  let name        = "Risk Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let
        rrs = rights $ V.toList $ V.map
          (riskRatio . pairwiseStudyToComparison)
          studies
        estimates  = map ((\s -> (roundDouble s 4)) . point) rrs
        cils       = map ((\s -> (roundDouble s 4)) . lower . ci) rrs
        cius       = map ((\s -> (roundDouble s 4)) . upper . ci) rrs
        correctRRs = [0.7500, 0.8000, 0.7368, 0.3125, 0.7273, 0.8889]
        correctcil = [0.3858, 0.3524, 0.3976, 0.2039, 0.3273, 0.4982]
        correctciu = [1.4581, 1.8162, 1.3655, 0.4790, 1.6159, 1.5861]
      if estimates == correctRRs
        then if (correctcil == cils) && (correctciu == cius)
          then return $ testPassed name $ "passed!"
          else
            return
            $ testFailed name
            $ ( "wrong RRs CIs"
              , show cils
              <> show correctcil
              <> " "
              <> show cius
              <> show correctciu
              )
        else
          return
          $ testFailed name
          $ ("wrong RRs", show estimates <> show correctRRs)

testOR :: IO Test
testOR = do
  let name        = "Odds Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let
        ors = rights $ V.toList $ V.map
          (oddsRatio . pairwiseStudyToComparison)
          studies
        estimates  = map ((\s -> (roundDouble s 4)) . point) ors
        cils       = map ((\s -> (roundDouble s 4)) . lower . ci) ors
        cius       = map ((\s -> (roundDouble s 4)) . upper . ci) ors
        correctORs = [0.6934, 0.7500, 0.6810, 0.2667, 0.6591, 0.8526]
        correctcil = [0.2984, 0.2612, 0.3143, 0.1661, 0.2329, 0.3895]
        correctciu = [1.6114, 2.1534, 1.4755, 0.4280, 1.8650, 1.8662]
      if estimates == correctORs
        then if (correctcil == cils) && (correctciu == cius)
          then return $ testPassed name $ "passed!"
          else
            return
            $ testFailed name
            $ ( "wrong ORs CIs"
              , show cils
              <> show correctcil
              <> " "
              <> show cius
              <> show correctciu
              )
        else
          return
          $ testFailed name
          $ ("wrong ORs", show estimates <> show correctORs)


testRD :: IO Test
testRD = do
  let name        = "Risk Difference"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let rds = rights $ V.toList $ V.map
            (riskDifference . pairwiseStudyToComparison)
            studies
          estimates  = map ((\s -> (roundDouble s 4)) . point) rds
          cils       = map ((\s -> (roundDouble s 4)) . lower . ci) rds
          cius       = map ((\s -> (roundDouble s 4)) . upper . ci) rds
          correctRDs = [-0.0615, -0.0500, -0.0625, -0.1375, -0.0750, -0.0308]
          correctcil = [-0.2025, -0.2327, -0.1875, -0.1833, -0.2608, -0.1818]
          correctciu = [0.0794, 0.1327, 0.0625, -0.0917, 0.1108, 0.1202]
      if estimates == correctRDs
        then if (correctciu == cius)
          then if (correctcil == cils)
            then return $ testPassed name $ "passed!"
            else return $ testFailed
              name
              (show correctcil, "\n wrong RDs CIs lower" <> show cils)
          else return $ testFailed
            name
            (show correctciu, "\n wrong RDs CIs upper" <> show cius)
        else
          return
          $ testFailed name
          $ (show correctRDs, "wrong RDs" <> show estimates)

reverseRD :: IO Test
reverseRD = do
  let name        = "Reverse Risk Difference"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let rds = rights $ V.toList $ V.map
            (riskDifference . pairwiseStudyToComparison)
            studies
          estimates  = map ((\s -> (roundDouble s 4)) . point) rds
          cils       = map ((\s -> (roundDouble s 4)) . lower . ci) rds
          cius       = map ((\s -> (roundDouble s 4)) . upper . ci) rds
          correctRDs = [0.0615, 0.0500, 0.0625, 0.1375, 0.0750, 0.0308]
          correctcil = [-0.0794, -0.1327, -0.0625, 0.0917, -0.1108, -0.1202]
          correctciu = [0.2025, 0.2327, 0.1875, 0.1833, 0.2608, 0.1818]
      if estimates == correctRDs
        then if (correctciu == cius)
          then if (correctcil == cils)
            then return $ testPassed name $ "passed!"
            else return $ testFailed
              name
              (show correctcil, "\n wrong RDs CIs lower" <> show cils)
          else return $ testFailed
            name
            (show correctciu, "\n wrong RDs CIs upper" <> show cius)
        else
          return
          $ testFailed name
          $ (show correctRDs, "wrong RDs" <> show estimates)

ivstudies :: IO Test
ivstudies = do
  let name        = "turn csv to IV MDs by the studyToIVStudy function"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, pwstudies) -> do
      let
        studies = map pairwiseToStudy $ V.toList pwstudies
        Right mdstudies =
          sequence $ map (flip studyToIVStudy meanDifference) studies
        contrasts = map (\(IVStudy sid cnts) -> contrastsToList cnts) mdstudies
        effects   = map getEffectsOfIVStudy mdstudies
      print $ show studies
      print $ show mdstudies
      --I.writeFile "mdivs.json" (encodeToLazyText studies)
      print $ show contrasts
      print $ show effects
      return $ testPassed name $ "passed!"

reverseOR :: IO Test
reverseOR = do
  let name        = "reverse Odds Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let ors = map reverseEffect $ rights $ V.toList $ V.map
            (oddsRatio . pairwiseStudyToComparison)
            studies
          estimates  = map ((\s -> (roundDouble s 4)) . point) ors
          cils       = map ((\s -> (roundDouble s 4)) . lower . ci) ors
          cius       = map ((\s -> (roundDouble s 4)) . upper . ci) ors
          correctORs = [1.4422, 1.3333, 1.4684, 3.7500, 1.5172, 1.1729]
          correctcil = [0.6206, 0.4644, 0.6777, 2.3363, 0.5362, 0.5359]
          correctciu = [3.3515, 3.8282, 3.1814, 6.0193, 4.2933, 2.5672]
      if estimates == correctORs
        then if (correctcil == cils) && (correctciu == cius)
          then return $ testPassed name $ "passed!"
          else
            return
            $ testFailed name
            $ ( "wrong ORs CIs"
              , show cils
              <> show correctcil
              <> " "
              <> show cius
              <> show correctciu
              )
        else
          return
          $ testFailed name
          $ ("wrong ORs", show estimates <> show correctORs)

reverseRR :: IO Test
reverseRR = do
  let name        = "reverse Risk Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies =
        C.decodeByName csvData :: Either
            String
            (C.Header, V.Vector PairwiseStudy)
  case estudies of
    Left  err          -> return $ testFailed name $ ("error parsing csv", err)
    Right (_, studies) -> do
      let ors = map reverseEffect $ rights $ V.toList $ V.map
            (riskRatio . pairwiseStudyToComparison)
            studies
          estimates  = map ((\s -> (roundDouble s 4)) . point) ors
          cils       = map ((\s -> (roundDouble s 4)) . lower . ci) ors
          cius       = map ((\s -> (roundDouble s 4)) . upper . ci) ors
          correctRRs = [1.3333, 1.2500, 1.3571, 3.2000, 1.3750, 1.1250]
          correctcil = [0.6858, 0.5506, 0.7324, 2.0875, 0.6189, 0.6305]
          correctciu = [2.5922, 2.8379, 2.5150, 4.9053, 3.0550, 2.0074]
      if estimates == correctRRs
        then if (correctcil == cils) && (correctciu == cius)
          then return $ testPassed name $ "passed!"
          else
            return
            $ testFailed name
            $ ( "wrong RRs CIs"
              , show cils
              <> show correctcil
              <> " "
              <> show cius
              <> show correctciu
              )
        else
          return
          $ testFailed name
          $ ("wrong RRs", show estimates <> show correctRRs)
