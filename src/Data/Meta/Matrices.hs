{-|
Module      : Data.Meta.Multiarm
Description : Matrix function for graphs
Copyright   : (c) Thodoris Papakonstantinou, 2019
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

-}

module Data.Meta.Matrices
  ( createBMatrix
  , createAMatrix
  , createCMatrix
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
import qualified Data.Graph.AdjacencyList.BFS      as BFS

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

createAMatrix :: G.Graph -> LA.Matrix Double
createAMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      rowfromEdge e =
          let
            (u, v) = G.toTuple e
            r      = LA.fromList $ replicate (nvs - 1) 0.0 :: LA.Vector Double
            r' =
              LAD.mapVectorWithIndex
                (\i _ ->
                  if i == u - 2 then 1.0 else if i == v - 2 then (-1.0) else 0.0
                )
                r :: LA.Vector Double
           in r'
      rows = map rowfromEdge es
      a    = LA.fromRows rows
  in  a

-- | create the Vertex Edge adjacency matrix given a graph

-- | create the Vertex Edge adjacency matrix given a graph
createCMatrix :: G.Graph -> LA.Matrix Double
createCMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      rowfromVertex v =
        let r = map (\e -> 
              let (u, v') = G.toTuple e
                  fillcell x | x==u = (-1.0)
                  fillcell x | x==v' = (1.0)
                  fillcell _ = (0.0)
               in  fillcell v
                  ) es
         in LA.fromList $ r :: LA.Vector Double
      rows = map rowfromVertex vs
      c    = LA.fromRows rows
  in  c

-- | create the Vertex Edge adjacency matrix given a graph
createXMatrix :: G.Graph -> LA.Matrix Double
createXMatrix g =
  let vs  = G.vertices g
      nvs = length vs
      es  = G.edges g
      nes = length es
      firstVertex = head vs
      bfsNet = BFS.bfs g firstVertex
      spanningtree = BFS.spanningTree bfsNet
      rowfromVertex v =
        let r = map (\e -> 
              let (u, v') = G.toTuple e
                  fillcell x | x==u = (-1.0)
                  fillcell x | x==v' = (1.0)
                  fillcell _ = (0.0)
               in  fillcell v
                  ) es
         in LA.fromList $ r :: LA.Vector Double
      rows = map rowfromVertex vs
      c    = LA.fromRows rows
  in  c
