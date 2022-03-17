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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Meta.NMA
  ( NetworkEffects (..)
  , Spring (..)
  , springNMA
  )
where

import           Control.Applicative
import           Data.List
import Data.Tuple
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.Either
import qualified Data.IntMap.Strict as IM

import qualified Numeric.LinearAlgebra         as LA

import qualified Data.Graph.AdjacencyList      as G
import qualified Data.Graph.AdjacencyList.BFS      as BFS

import           Data.Meta.Effects
import Data.Meta.Matrices
import Data.Meta.Studies

data Effect a => NetworkEffects a = 
  NetworkEffects { directs :: StudyGraph a
                 --, leagueTable :: LA.Matrix Double
                 --, lstar :: Map.Map G.Edge (Either Spring Spring)
                 --, lstar :: Map.Map G.Edge Double
                 , networkEstimates :: Map.Map TreatmentId (Map.Map TreatmentId Double)
                 , networkVariances :: Map.Map TreatmentId (Map.Map TreatmentId Double)
                 , debugmsg :: String
                 }
  deriving (Show, Eq)

-- | Matrices needed for solving the linear system
data Matrices = Matrices { kMatrix :: LA.Matrix Double
                         , llMatrix :: [Double]
                         , cMatrix :: LA.Matrix Double
                         , dMatrix :: LA.Matrix Double
                         , xMatrix :: LA.Matrix Double
                         , spnTr :: Set.Set G.Edge
                         , parents :: IM.IntMap G.Vertex
                         , vss :: [G.Vertex]
                         , directEdges :: Set.Set G.Edge
                         }

type NaturalLength = Double
type Hardness = Double
type SpringLength = Double

data Spring = Spring Hardness NaturalLength SpringLength
  deriving (Show, Eq)

reverseSpring :: Spring -> Spring
reverseSpring (Spring k ll l) = Spring k (-ll) (-l)

hardness :: Spring -> Hardness
hardness (Spring k _ _) = k

naturalLength :: Spring -> NaturalLength
naturalLength (Spring _ ll _) = ll

springLength :: Spring -> SpringLength
springLength (Spring _ _ l) = l

type TauSquare = Double

springNMA :: (ArmEffect a b, Gaussian a, Gaussian b) => [Study]
                             -> Maybe G.Vertex
                             -> Maybe TauSquare
                             -> (Arm -> Either String a)
                             -> Either String (NetworkEffects b)
springNMA studies mpinnedVertex mtau2 armEffect = 
  let vs = foldl' (\acvs study -> 
                    let sid = getStudyId study
                        arms = getStudyArms study
                     in case mtau2 of
                          Nothing -> Set.insert (Left sid) $ Set.union acvs 
                            $ Set.fromList $ map (Right . tidOfArm) arms
                          Just tau2 -> 
                            let tauvs = Set.fromList $ map (\a 
                                      -> Left $ StudyId $ StringId ((show sid) ++ "_" ++ (show $ tidOfArm a))) arms
                                tidvs = Set.fromList $ map (Right . tidOfArm) arms
                             in Set.insert (Left sid) $ Set.union acvs $ Set.union tauvs tidvs
                  ) Set.empty studies  :: Set.Set (Either StudyId TreatmentId)
      vssts = IM.fromList $ zip [1..] $ Set.toList vs
      stsvs = Map.fromList $ map swap $ IM.toList vssts
      Right vsts = sequence $ IM.filter isRight vssts -- only treatment vertices
      tsvs = Map.fromList $ map swap $ IM.toList vsts
      esprings = foldl' (\eaces study -> 
                    let sid = getStudyId study
                        arms = getStudyArms study
                        studysprings = sequence $  case armEffect $ head arms of
                          Left err -> [Left err]
                          Right aArm -> 
                            let pArm = expectation aArm
                             in foldl' (\acc a -> case armEffect a of
                                        Left err -> Left err : acc
                                        Right armef -> 
                                          let tid = tidOfArm a
                                              tausid = StudyId $ StringId ((show sid) ++ "_" ++ show tid)
                                              vs = stsvs Map.! (Left sid)
                                              vt = stsvs Map.! (Right tid)
                                              vtau = stsvs Map.! (Left tausid)
                                              eff = relatedRelative armef
                                              releff = translate eff (-pArm)
                                              nl = expectation releff
                                              k = 1 / (variance releff)
                                              spr = Spring k nl nl
                                           in  case mtau2 of
                                                Nothing -> 
                                                  let sttrEdge = G.Edge vs vt
                                                   in (Right (sttrEdge, spr)) : acc
                                                Just tau2 -> 
                                                  let sttauEdge = G.Edge vs vtau
                                                      sttauspr = Spring (2/tau2) 0 0 -- That's where tau2 is inserted
                                                      tautrEdge = G.Edge vtau vt
                                                   in [Right (sttauEdge, sttauspr)
                                                      , Right (tautrEdge, spr)] <> acc
                                       ) [] arms :: [Either String (G.Edge, Spring)]
                     in case eaces of 
                          Left err -> Left err
                          Right aces -> 
                             case studysprings of
                               Left err -> Left err
                               Right sprs -> Right (sprs ++ aces)
              ) (Right []) studies :: Either String [(G.Edge,Spring)]
    in case esprings of 
       Left err -> Left err
       Right springs -> 
         let springsMap = Map.fromList springs
             es = Map.keys springsMap
             numEdges = length es
             directedSG = G.graphFromEdges es
             undirectedSG = G.makeUndirected directedSG
             getSpring :: G.Edge -> Spring -- from undirected edge to spring
             getSpring e =
               case Map.lookup e springsMap of
                 Nothing -> reverseSpring $ springsMap Map.! (G.reverseEdge e)
                 Just sp -> sp
             pinnedV = 
               case mpinnedVertex of
                 Just p -> p
                 Nothing -> 1
             -- | get the Matrices corresponding to defined pinned Vertex
             getMatrices :: G.Vertex -> Matrices 
             getMatrices pinnedVertex = 
             -- | Networks spanning tree
               let netbfs = BFS.bfs undirectedSG pinnedVertex
                   spnTr = Set.fromList $ BFS.spanningTree netbfs
                   parents = BFS.parent netbfs
                   vss = map (\(G.Edge u v)->v) (Set.toList spnTr)
                   llmap = Map.fromList $ map (\e ->
                              case Set.member e spnTr of
                                True -> (e, Right $ getSpring e)
                                False -> let re = G.reverseEdge e 
                                          in case Set.member re spnTr of
                                               True -> (re, Right $ getSpring re)
                                               False -> (e, Left $ getSpring e)
                                        ) es
                   -- | L vector (E><1) with the natural lengths of springs which  respect
                   -- the spanning tree orientation
                   directEdges = Set.fromList $ Map.keys llmap
                   llMatrix = map (\de -> 
                     let ms = llmap Map.! de 
                      in case ms of 
                          Left sp -> naturalLength sp
                          Right sp -> naturalLength sp)
                              $ Set.toList directEdges
                   -- weight matrix (E x E)
                   kMatrix = LA.diagl $ map (hardness . getSpring) $ Set.toList directEdges
                   -- cMatrix (V-1 x E) 
                   cMatrix = 
                     let vertexToRow :: G.Vertex -> [Double]
                         vertexToRow x = map (\(G.Edge u v) -> 
                                         if x==u
                                            then 1.0
                                            else
                                             if x==v then (-1)
                                                     else 0
                                             )
                                       $ Set.toList directEdges
                      in LA.fromLists $ map vertexToRow vss
                   -- D matrix (V-1 x E)
                   dMatrix = cMatrix LA.<> kMatrix
                   -- X matrix (E x (V-1)) consistency matrix
                   -- Definition: X <> L* = L
                   xMatrix = 
                     let edgeToRow :: G.Edge -> [Double]
                         edgeToRow (G.Edge u v) =
                           let r = replicate (length vss) 0
                               r' = IM.fromList $ zip vss r
                               followEnd :: G.Vertex 
                                         -> IM.IntMap Double
                                         -> IM.IntMap Double
                               followEnd k r =
                                 if k == pinnedVertex 
                                    then r
                                    else
                                      let r' = IM.adjust (+ 1) k r
                                          k' = (BFS.parent netbfs) IM.! k
                                       in followEnd k' r'
                               followStart k r =
                                 if k == pinnedVertex 
                                    then r
                                    else
                                      let r' = IM.adjust (+ (-1)) k r
                                          k' = (BFS.parent netbfs) IM.! k
                                       in followStart k' r'
                               r'' = followEnd v $ followStart u r'
                            in map (\v -> r'' IM.! v) vss -- has to respect vss ordering
                      in LA.fromLists $ map edgeToRow $ Set.toList directEdges
                in Matrices { kMatrix
                            , llMatrix
                            , cMatrix
                            , dMatrix
                            , xMatrix
                            , parents
                            , spnTr 
                            , vss
                            , directEdges
                            }
             -- | l* (V-1 >< 1) where V-1 are the end vertices of the spanning
             -- tree
             mtrcs = getMatrices pinnedV
             kMatrix'=  kMatrix mtrcs 
             llMatrix'= llMatrix mtrcs 
             cMatrix'=  cMatrix mtrcs 
             dMatrix'=  dMatrix mtrcs 
             xMatrix'=  xMatrix mtrcs 
             parents'=  parents mtrcs 
             spnTr'  =  spnTr mtrcs 
             vss' = vss mtrcs
             directEdges' = directEdges mtrcs
             lstar = ((LA.inv (dMatrix' LA.<> xMatrix')) LA.<> dMatrix') LA.<> (numEdges LA.>< 1 $ llMatrix')
             lstarMap = Map.fromList $ zip (Set.toList spnTr') (LA.toList $ LA.flatten lstar)
             getLength :: G.Edge 
                       -> G.Vertex -- pinned Vertex
                       -> Map.Map G.Edge Double -- lstarMap
                       -> IM.IntMap G.Vertex -- parents
                       -> NaturalLength
             -- | follows back the spanning tree adding parent edges' length
             getLength (G.Edge u v) pinnedV lstarMap parents =
               let followEnd :: G.Vertex 
                       -> Double
                       -> Double
                   followEnd k l =
                       if k == pinnedV
                          then l
                          else
                            let k' = parents IM.! k
                                l' = lstarMap Map.! (G.Edge k' k)
                             in followEnd k' (l+l')
                   followStart k l =
                       if k == pinnedV
                          then l
                          else
                            let k' = parents IM.! k
                                l' = lstarMap Map.! (G.Edge k' k)
                             in followStart k' (l-l')
                in if u == v 
                      then 0
                      else 
                        followEnd v $ followStart u 0
             netests = foldl' (\acc utid ->
               let vmap = foldl' 
                     (\mac vtid -> 
                       let u = tsvs Map.! utid
                           v = tsvs Map.! vtid 
                           l = getLength (G.Edge u v) pinnedV lstarMap parents'
                        in Map.insert vtid l mac
                     ) Map.empty $ Map.keys tsvs
                in Map.insert utid vmap acc
                   ) Map.empty $ Map.keys tsvs
             effectiveVariance :: G.Edge 
                               -> G.Vertex -- pinned
                               -> [G.Vertex] -- vss
                               -> Set.Set G.Edge -- spanning tree
                               -> LA.Matrix Double -- D matrix
                               -> LA.Matrix Double -- X matrix
                               -> IM.IntMap G.Vertex -- parents
                               -> Hardness
             effectiveVariance (G.Edge u v) pinnedV vss spnTr dMatrix xMatrix parents = 
               let r = IM.fromList $ zip vss $ replicate (length vss) 0
                   r' = case IM.lookup u r of 
                               Nothing -> r
                               Just _ -> IM.insert u (-1.0) r
                   r'' = case IM.lookup v r' of 
                               Nothing -> r'
                               Just _ -> IM.insert v (1.0) r'
                   forceVector = LA.tr' $ LA.fromLists [map (\v -> r'' IM.! v) vss] -- has to respect vss ordering
                   eflstar = (LA.inv (dMatrix LA.<> xMatrix)) LA.<> forceVector
                   eflstarMap = Map.fromList $ 
                     zip (Set.toList spnTr) (LA.toList $ LA.flatten eflstar)
                in abs $ getLength (G.Edge u v) pinnedV eflstarMap parents
             netvars = foldl' (\acc utid ->
               let vmap = foldl' 
                     (\mac vtid -> 
                       let u' = tsvs Map.! utid
                           v' = tsvs Map.! vtid 
                           u = min u' v'
                           v = max u' v'
                           mtc = getMatrices u 
                           l = if u == v
                                  then 0 
                                  else
                                  --effectiveVariance (G.Edge u v) u (vss mtc) (spnTr mtc) (dMatrix mtc) (xMatrix mtc) (parents mtc)
                                  effectiveVariance (G.Edge u v) pinnedV (vss') (spnTr') (dMatrix') (xMatrix') (parents')
                        in Map.insert vtid l mac
                     ) Map.empty $ Map.keys tsvs
                in Map.insert utid vmap acc
                   ) Map.empty $ Map.keys tsvs
             sg = StudyGraph { studyGraph = G.graphFromEdges []
                             , directGraph = G.graphFromEdges $ Set.toList spnTr'
                             , vsts = vsts
                             , tsvs = tsvs
                             }
             nets = Map.empty
          in Right $ NetworkEffects { directs = sg
                                    --, leagueTable = dMatrix'
                                    --, lstar = lstar
                                    , networkEstimates = netests
                                    , networkVariances = netvars
                                    , debugmsg = unlines [ "spnTr " <> (show $ Set.toList spnTr')
                                                         , "vss " <> (show $ map (\(G.Edge u v)->v) (Set.toList spnTr'))
                                                         , "netvars" <> (show netvars)
                                                         , "dMatrix" <> (show dMatrix')
                                                         , "directEdges" <> (show directEdges')
                                                         ]
                                    }


