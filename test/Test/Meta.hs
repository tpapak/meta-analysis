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
          {-, oddsratio-}
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
      putStrLn "Studies file"
      putStrLn $ show studies
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
      putStrLn "Studies file"
      putStrLn $ show studies
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
      putStrLn "mean differences of continuous"
      putStrLn $ show $ rights meandiffs
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
      putStrLn "standardized mean differences of continuous"
      let smds = rights $ V.toList $ V.map standardizedMeanDifference studies
          gs   = map ((\s -> (roundDouble s 3)) . effect) smds
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

{-oddsratio :: IO Test-}
{-oddsratio = do-}
  {-let name = "odds ratio"-}
  {-let studiesFile = "test/binary.csv"-}
  {-csvData <- B.readFile studiesFile-}
  {-let estudies = C.decodeByName csvData-}
               {-:: Either String (C.Header, V.Vector Study)-}
  {-case estudies of-}
    {-Left err -> return $ testFailed name $ ("error parsing csv",err)-}
    {-Right (_, studies) -> do-}
      {-let meandiffs = V.toList $ V.map meanDifference studies-}
      {-putStrLn "mean differences of continuous"-}
      {-putStrLn $ show $ rights meandiffs-}
      {-return $ testPassed name $ "passed!"-}
