{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Test.Meta.Studies where

import           Data.Maybe

import           Data.Aeson
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import qualified Data.ByteString.Lazy as B
import           Data.Either

import           TestHS

import           Data.Numerics
import Data.Graph.AdjacencyList
import           Data.Meta.Effects
import           Data.Meta.Studies

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests =
  [ continuouswide
  , continuouslong
  , binarywide
  , binarylong
  ]

continuouswide :: IO Test
continuouswide = do
  let name        = "read continuous wide study"
  let studiesFile = "test/continuous.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      return $ testPassed name $ "passed!"

continuouslong :: IO Test
continuouslong = do
  let name        = "read continuous long study"
  let studiesFile = "test/continuouslong.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      --print studies
      return $ testPassed name $ "passed!"

binarywide :: IO Test
binarywide = do
  let name        = "read binary wide study"
  let studiesFile = "test/binary.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      return $ testPassed name $ "passed!"

binarylong :: IO Test
binarylong = do
  let name        = "read Diabetes: binary long study"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      --print studies
      return $ testPassed name $ "passed!"
