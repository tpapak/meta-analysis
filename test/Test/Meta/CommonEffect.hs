module Test.Meta.CommonEffect where

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
import           Data.Meta.CommonEffect

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ smd
          , rr
          ]

smd :: IO Test
smd = do
  let name = "Common Effect meta-analysis on continuous SMD"
  let studiesFile = "test/continuous.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let emds = rights $ fmap standardizedMeanDifference $ V.toList studies 
      let ce = commonEffect emds
      let (SMD e v) = ce
      let foundce = (mapEffect (\c -> roundDouble c 4) (SMD e v))
      let expected = SMD 0.4143 0.0041
      if  foundce == expected
        then
          return $ testPassed name $ "nice!!"
        else
          return $ testFailed name $ (show expected, show foundce)

rr :: IO Test
rr = do
  let name = "Common Effect meta-analysis on binary RR"
  let studiesFile = "test/binary.csv"
  csvData <- B.readFile studiesFile
  let estudies = C.decodeByName csvData
               :: Either String (C.Header, V.Vector Study)
  case estudies of
    Left err -> return $ testFailed name $ ("error parsing csv",err)
    Right (_, studies) -> do
      let emds = rights $ fmap riskRatio $ V.toList studies 
      let ce = commonEffect emds
      let foundce = (mapEffect (\c -> roundDouble c 4) ce)
      let expected = RR 0.5775 (CI 0.4511 0.7392)
      if  foundce == expected
        then
          return $ testPassed name $ "nice!!"
        else
          return $ testFailed name $ (show expected, show foundce)
