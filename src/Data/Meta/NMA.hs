{-|
Module      : Data.Meta.NMA
Description : Second stage NMA
Copyright   : (c) Thodoris Papakonstantinou, 2019
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Given a study graph of direct effects perform network-meta analysis
-}

module Data.Meta.NMA
  ( NetworkEffects (..)
  , commonEffectNMA
  )
where

import           Control.Applicative
import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.Either
import qualified Data.IntMap.Strict as IM

import qualified Numeric.LinearAlgebra         as LA
import qualified Numeric.LinearAlgebra.Devel   as LAD

import qualified Data.Graph.AdjacencyList      as G
import           Data.Meta.Effects
import Data.Meta.Matrices
import Data.Meta.Studies

data Effect a => NetworkEffects a = 
  NetworkEffects { directs :: StudyGraph a
                 , leagueTable :: LA.Matrix Double
                 , networkEstimates :: Map.Map ComparisonId a
                 }
  deriving (Show, Eq)

--leagueTable :: Effect a => NetworkEffects a -> Map.Map ComparisonId a
--
commonEffectNMA :: (Gaussian a, Effect a) => StudyGraph a 
                -> Maybe TreatmentId
                -> NetworkEffects a
commonEffectNMA sg mreferenceTreatment =
  let treatments = Map.keys $ tsvs sg
      refTr = case mreferenceTreatment of
                Nothing -> head treatments
                Just t -> t
      vertices = IM.keys $ vsts sg
      directEdges = G.edges $ directGraph sg
      aMatrix = createAMatrix $ directGraph sg :: LA.Matrix Double
      edgeToComparison (G.Edge u v) = 
          let ta = fromJust $ IM.lookup u (vsts sg)
              tb = fromJust $ IM.lookup v (vsts sg)
           in ComparisonId ta tb
      weights = map 
        (\e -> let v = (variance $  
                     fromJust $ Map.lookup (edgeToComparison e) (directEffects sg))
                in if v < 1E-8
                      then 0
                      else 1 / v
        ) directEdges
      points = map 
        (\e -> expectation $  
                 fromJust $ Map.lookup (edgeToComparison e) (directEffects sg)
        ) directEdges
      kMatrix = LA.diagl weights
      lVector = LA.vector points 
      --xVector = exp $ ((LA.pinvTol 1E8 $ kMatrix <> aMatrix) <> kMatrix) LA.#> lVector
      a = (LA.tr' aMatrix) <> kMatrix <> aMatrix
      b = ((LA.tr' aMatrix) <> kMatrix) LA.#> lVector
      -- Gertas
      --xVector = exp $ (bMatrix <> (LA.pinvTol 1E8 $ (LA.tr' bMatrix) <> kMatrix <> bMatrix) 
              -- <> (LA.tr' bMatrix) <> kMatrix) LA.#> lVector
      xVector = exp $ a LA.<\> b
      neteffects = Map.empty
   in NetworkEffects { directs = sg
                     , leagueTable = LA.diag xVector
                     , networkEstimates = neteffects
                     }
