module Test.Meta where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy             as B
import qualified Data.Map.Strict                  as Map
import qualified Data.Vector as V
import qualified Data.Csv             as C
import Data.Either

import           TestHS

import           Data.Meta

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ test1
          , test2
          , test3
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
