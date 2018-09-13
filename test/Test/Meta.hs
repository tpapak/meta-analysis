module Test.Meta where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy             as B
import qualified Data.Map.Strict                  as Map
import qualified Data.Vector as V
import qualified Data.Csv             as C
import Data.Either

import           TestHS

import Data.Numerics
import           Data.Meta

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ test1
          , test2
          , test3
          , test4
          , testLogOR
          , testRR
          , testOR
          ]

test1 :: IO Test
test1 = do
  let name = "read continuous study"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeWith C.defaultDecodeOptions C.HasHeader csvData
               :: Either String (V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right studies -> do
      {-putStrLn "Studies file"-}
      {-putStrLn $ show studies-}
      return $ testPassed name $ "passed!"

test2 :: IO Test
test2 = do
  let name = "read binary study"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      {-putStrLn "Studies file"-}
      {-putStrLn $ show studies-}
      return $ testPassed name $ "passed!"

test3 :: IO Test
test3 = do
  let name = "mean differences"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let meandiffs = V.toList $ V.map meanDifference studies
      {-putStrLn "mean differences of continuous"-}
      {-putStrLn $ show $ rights meandiffs-}
      return $ testPassed name $ "passed!"

test4 :: IO Test
test4 = do
  let name = "standardized mean differences"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      {-putStrLn "standardized mean differences of continuous"-}
      let smds = rights $ V.toList $ V.map standardizedMeanDifference studies
          gs   = map ((\s -> (roundDouble s 3)) . expectation) smds
          vgs  = map ((\s -> (roundDouble s 3)) . variance) smds
          correctgs = [ 0.095
                      , 0.277
                      , 0.367
                      , 0.664
                      , 0.462
                      , 0.185
                      ]
          correctvgs = [ 0.033
                       , 0.031
                       , 0.050
                       , 0.011
                       , 0.043
                       , 0.023
                       ]
      if gs == correctgs && vgs == correctvgs
        then
          return $ testPassed name $ "passed!"
        else
          return $ testFailed name $ ("wrong swithin values", (show gs <> show correctgs <> show vgs <> show correctvgs))

testLogOR :: IO Test
testLogOR = do
  let name = "log Odds Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let lnORs = rights $ V.toList $ V.map logOddsRatio studies
          estimates  = map ((\s -> (roundDouble s 4)) . expectation) lnORs
          variances  = map ((\s -> (roundDouble s 4)) . variance) lnORs
          correctlnORs = [ -0.3662
                         , -0.2877
                         , -0.3842
                         , -1.3218
                         , -0.4169
                         , -0.1595
                         ]
          correctvars = [ 0.1851
                        , 0.2896
                        , 0.1556
                        , 0.0583
                        , 0.2816
                        , 0.1597
                        ]
      if estimates == correctlnORs 
         then
           if variances == correctvars 
              then
                return $ testPassed name $ "passed!"
              else
                return $ testFailed name $ ("wrong lnRRs variances",show variances <> show correctvars)
         else
           return $ testFailed name $ ("wrong lnRRs",show estimates <> show correctlnORs)

testRR :: IO Test
testRR = do
  let name = "Risk Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let rrs = rights $ V.toList $ V.map riskRatio studies
          estimates  = map ((\s -> (roundDouble s 4)) . effect) rrs
          cils  = map ((\s -> (roundDouble s 4)) . lower . ci) rrs
          cius  = map ((\s -> (roundDouble s 4)) . upper . ci) rrs
          correctRRs = [ 0.7500
                       , 0.8000
                       , 0.7368
                       , 0.3125
                       , 0.7273
                       , 0.8889
                       ]
          correctcil = [ 0.3858
                       , 0.3524
                       , 0.3976
                       , 0.2039
                       , 0.3273
                       , 0.4982
                       ]
          correctciu = [ 1.4581                      
                       , 1.8162                      
                       , 1.3655                      
                       , 0.4790                      
                       , 1.6159                      
                       , 1.5861
                       ] 
      if estimates == correctRRs 
         then
           if (correctcil == cils) && (correctciu == cius)
              then
                return $ testPassed name $ "passed!"
              else
                return $ testFailed name $ ("wrong RRs CIs",show cils <> show correctcil <> " " <>show cius <> show correctciu)
         else
           return $ testFailed name $ ("wrong RRs",show estimates <> show correctRRs)

testOR :: IO Test
testOR = do
  let name = "Odds Ratio"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let ors = rights $ V.toList $ V.map oddsRatio studies
          estimates  = map ((\s -> (roundDouble s 4)) . effect) ors
          cils  = map ((\s -> (roundDouble s 4)) . lower . ci) ors
          cius  = map ((\s -> (roundDouble s 4)) . upper . ci) ors
          correctORs = [ 0.6934 
                      , 0.7500 
                      , 0.6810 
                      , 0.2667 
                      , 0.6591 
                      , 0.8526 
                       ]
          correctcil = [ 0.2984
                       , 0.2612
                       , 0.3143
                       , 0.1661
                       , 0.2329
                       , 0.3895
                       ]
          correctciu = [ 1.6114
                       , 2.1534
                       , 1.4755
                       , 0.4280
                       , 1.8650
                       , 1.8662
                       ] 
      if estimates == correctORs
         then
           if (correctcil == cils) && (correctciu == cius)
              then
                return $ testPassed name $ "passed!"
              else
                return $ testFailed name $ ("wrong ORs CIs",show cils <> show correctcil <> " " <>show cius <> show correctciu)
         else
           return $ testFailed name $ ("wrong ORs",show estimates <> show correctORs)

