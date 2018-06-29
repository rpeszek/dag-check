{-# LANGUAGE 
   ScopedTypeVariables
#-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

----------------------------------------------------
-- |
-- Simple leaf calculaton based on topological sort
-- conceputal code for my work
----------------------------------------------------
module Dag.Leaves (
  leaves
) where

import             Dag 
import qualified   Data.HashMap.Lazy as HM
import qualified   Data.HashSet as HS
import             Data.Hashable


type WorkStore v = HM.HashMap v (HS.HashSet v)

-- | given vertex 'v' calculate all reachable leaf vertices 
leaves :: forall g v e. (Dag g v e, Eq v, Hashable v) => g -> v -> Maybe (HS.HashSet v)
leaves graph root = HM.lookup root $ foldr collect HM.empty (topoSort graph)
     where 
       -- | aggregate leaves for all successors and adds the calculation 
       -- under current vertex
       collect :: v -> WorkStore v -> WorkStore v
       collect vert acc = 
           case successors graph vert of 
               Nothing -> error "not in graph"
               Just [] -> HM.insert vert (HS.singleton vert) acc
               Just succs -> HM.insert vert (aggrSuccessors succs acc) acc
       -- | simply aggregate already computed leaves for given set of keys
       aggrSuccessors :: [v] -> WorkStore v -> HS.HashSet v
       aggrSuccessors succs acc = foldr (\v vs -> vs `HS.union` HM.lookupDefault HS.empty v acc) HS.empty succs 
     
