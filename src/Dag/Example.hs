{-# LANGUAGE 
   MultiParamTypeClasses
  , FlexibleInstances
#-}

-------------------------------------------------------
-- Simple implementation of Dag typeclass
-------------------------------------------------------

module Dag.Example where
import Dag

type TopSortedVs v = [v]

{-| Represents general directed graph -}
data SampleDiGraph v e = SampleDiGraph {
   sortedVerts :: TopSortedVs v
  , edges :: [e]
} deriving Show

instance (DiEdge v e, Eq v) => Dag (SampleDiGraph v e) v e where
   -- | topoSort :: g -> [v]
   topoSort (SampleDiGraph verts _) = verts
   -- | resolveDEdge ::  g -> e -> Maybe (v,v)
   -- strictly not correct becasue it does not check if edge belongs to graph
   resolveDEdge _ = Just . resolveDiEdge 
   -- | dEdgeFrom :: g -> v -> Maybe [e]
   dEdgeFrom g@(SampleDiGraph _ dedges) = dEdgeFromDefault dedges g  
   -- | dEdges :: g -> [e]
   dEdges(SampleDiGraph _ dedges) = dedges

empty :: SampleDiGraph v e 
empty = SampleDiGraph [] []

singleton ::  v -> SampleDiGraph v e
singleton vx = SampleDiGraph [vx] [] 

disconnected ::  [v] -> SampleDiGraph v e
disconnected vx = SampleDiGraph vx [] 
