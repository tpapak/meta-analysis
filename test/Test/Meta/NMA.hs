module Test.Meta.NMA where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import qualified Data.Csv                      as C
import           Data.Either

import           TestHS

import           Data.Numerics
import           Data.Meta.Studies
import           Data.Meta.Effects
import           Data.Meta.Pairwise.CommonEffect
import           Data.Meta.NMA

ioTests :: [IO Test]
ioTests = [
          diabetesCE
          ]

diabetesCE :: IO Test
diabetesCE = do
  let name = "NMA Diabetes common effect OR"
  let studiesFile = "test/diabetes.json"
  estudies <- readStudies studiesFile
  case estudies of
    Left err -> return $ testFailed name $ ("Error merging arms to studies", err)
    Right studies -> do
      let Right logors = sequence $ map (flip studyToIVStudy logOddsRatio) studies
          estudiesgraph = studiesGraph' logors commonEffect
       in case estudiesgraph of
            Left grerr -> return $ testFailed name $ ("error in synthesizing studies", grerr)
            Right studiesgraph -> do
                let netes = commonEffectNMA studiesgraph Nothing
                    mlor = Map.lookup (ComparisonId 
                                           (TreatmentId (StringId "Placebo"))
                                           (TreatmentId (StringId "ACE"))) 
                                           $ networkEstimates netes
                 in case mlor of
                       Nothing -> return $ testFailed name $ ("no nets", show netes)
                       Just lor ->
                             let founde  = mapEstimate (flip roundDouble 3) (logORToOR lor)
                                 expected = OR 1.126 (CI 1.037 1.221)
                              in if expected == founde
                                   then return $ testPassed name $ show founde <> "passed!"
                                   else return $ testFailed name $ (show expected, show founde)

