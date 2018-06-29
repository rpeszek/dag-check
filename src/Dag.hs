{-# LANGUAGE 
   MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
#-}

-----------------------------------------------------------
-- Simple typeclass definition (not very type safe) of a Dag  
-----------------------------------------------------------
module Dag where
import Control.Monad (join)
import Data.Maybe (fromMaybe)
  
-- | use of Nothing signifies that the search element is not in the graph
class Dag g v e | g -> v, g -> e where
    topoSort :: g -> [v]
    resolveDEdge ::  g -> e -> Maybe (v,v)
    dEdgeFrom :: g -> v -> Maybe [e]
    dEdges :: g -> [e]
    dEdges g = join . map (fromMaybe [] . dEdgeFrom g) $ topoSort g

-- | returns Nothing if v is not in the graph
isLeaf :: Dag g v a => g -> v -> Maybe Bool
isLeaf graph = fmap (null) . dEdgeFrom graph -- ^ null tests if list is empty

successors :: Dag g v e => g -> v -> Maybe [v]
successors g vert = do 
            fromArrows <- dEdgeFrom g vert
            mapM (fmap snd . resolveDEdge g) fromArrows

dEdgeFromDefault :: (Dag g v e, Eq v) => [e] -> g -> v -> Maybe [e]
dEdgeFromDefault arrs graph vert = 
       let matchedArrows = filter ((Just vert ==) . fmap (fst) . resolveDEdge graph) arrs
           found = elem vert (topoSort graph)
       in if found 
          then Just matchedArrows
          else Nothing

{- Other: mapping between vertices and edges -}
class DiEdge v e where
  resolveDiEdge :: e -> (v,v)

instance DiEdge a (a,a) where
  resolveDiEdge = id
