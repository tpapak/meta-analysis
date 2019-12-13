{-|
Module      : Data.Meta.Multiarm
Description : Reweight multi-arm studies
Copyright   : (c) Thodoris Papakonstantinou, 2019
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Following [Rücker's et al. 2013](10.1002/sim.6236) method for substituting a multiarm study with n choose 2 independent reweighted contrasts. The code is the haskell intetrpretation of netmeta's multiarm function
-}

module Data.Meta.Multiarm
  ( reweightMultiArm
  , createBMatrix
  , effectiveVarianceMatrix
  )
where

import           Control.Applicative
import           Data.List
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.Either
import           Data.Meta.Effects
import qualified Numeric.LinearAlgebra         as LA
import qualified Numeric.LinearAlgebra.Devel   as LAD
import qualified Data.Graph.AdjacencyList      as G


reweightMultiArm
  :: (Gaussian a, Effect a) => [Contrast a] -> Either String [Contrast a]
reweightMultiArm contrastList =
  let
    comparisonList = map (\(Contrast ta tb e) -> (ta, tb)) contrastList
    treatments     = foldl' (\vts (ta, tb) -> Set.insert tb (Set.insert ta vts))
                            Set.empty
                            comparisonList
    tsvs     = Map.fromList $ zip (Set.toList treatments) [1 ..]
    vsts     = Map.fromList $ zip [1 ..] (Set.toList treatments)
    edgeList = map
      (\(ta, tb) -> G.fromTuple
        (fromJust (Map.lookup ta tsvs), fromJust (Map.lookup tb tsvs))
      )
      comparisonList
    multigraph   = G.graphFromEdges edgeList
    completeG    = G.completeGraph $ G.numVertices multigraph
    varianceList = map
      (\(Contrast ta tb e) ->
        let
          ed = G.Edge (fromJust (Map.lookup ta tsvs))
                      (fromJust (Map.lookup tb tsvs))
        in  (ed, variance e)
      )
      contrastList
    reverseVarianceList = map (\(ed, v) -> (G.reverseEdge ed, v)) varianceList
    varianceMap         = Map.fromList $ union varianceList reverseVarianceList
    checkEnoughContrasts =
      isJust $ mapM (flip Map.lookup varianceMap) $ G.edges completeG
    checkMoreContrasts = Map.size varianceMap == 2 * length contrastList
  in
    if checkEnoughContrasts && checkMoreContrasts
      then
        let directedComplete = G.removeReverseEdges completeG
            bMatrix          = createBMatrix directedComplete
            variances        = map (\e -> fromJust $ Map.lookup e varianceMap)
              $ G.edges directedComplete
            erMatrix = effectiveVarianceMatrix variances bMatrix
         in case erMatrix of
              Left err -> Left err
              Right rMatrix ->
                --Lt <- -0.5 * t(B) %*% B %*% R %*% t(B) %*% B / k^2
                let lt = (-0.5) *
                      (LA.tr bMatrix)
                        <> bMatrix
                        <> rMatrix
                        <> (LA.tr bMatrix)
                        <> bMatrix / (fromIntegral $ G.numVertices directedComplete)^2
                    ll = LA.pinv lt
                    -- W <- diag(diag(L)) - L
                    wv = LA.diag (LA.takeDiag ll) - ll -- ^ Weight matrix
                    vv = 1 / wv -- ^ new variances
                    newContrasts = map (\(Contrast ta tb ef) 
                      -> let taindx = fromJust (Map.lookup ta tsvs)
                             tbindx = fromJust (Map.lookup tb tsvs)
                             newVariance = LA.atIndex vv (taindx - 1, tbindx -1)
                             newef = changeVariance ef newVariance
                          in (Contrast ta tb newef)
                     ) contrastList
                 in Right newContrasts
      else Left "wrong contrast number"


-- | create the edge vertex adjacency matrix given a graph
createBMatrix :: G.Graph -> LA.Matrix Double
createBMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      rowfromEdge e =
          let
            (u, v) = G.toTuple e
            r      = LA.fromList $ replicate nvs 0.0 :: LA.Vector Double
            r' =
              LAD.mapVectorWithIndex
                (\i _ ->
                  if i == u - 1 then 1.0 else if i == v - 1 then (-1.0) else 0.0
                )
                r :: LA.Vector Double
          in
            r'
      rows = map rowfromEdge es
      b    = LA.fromRows rows
  in  b

-- | Distribute the edge variances on a symmetrical matrix R given the variances
-- and the edge vertex adjacency matrix B
effectiveVarianceMatrix
  :: [Double] -> LA.Matrix Double -> Either String (LA.Matrix Double)
effectiveVarianceMatrix variances bMatrix =
  let v                 = LA.diagl variances
      nvars             = length variances
      correctDimensions = nvars == fst (LA.size bMatrix)
  in  if correctDimensions
        then
          let lp = LA.tr bMatrix <> v <> bMatrix
          in  Right $ LA.diag (LA.takeDiag lp) - lp
        else Left ("wrong dimensions" <> show variances <> show bMatrix)
