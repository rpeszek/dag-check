{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Dag.Eff.DFS
-- Description :  Standard (texbook) imperative implemenation of graph DFS 
--                with effectful programming treating in-place mutations as 
--                non-dismissable effects.
--                Uses freer-simple effects library.
--                See also similar `Dag.Eff.BFS`
----------------------------------------------------------------------------
module Dag.Eff.DFS where
  
import           Control.Monad.Freer       (Eff, Member, runM)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import qualified Dag.Eff                   as DS         -- mutating data store commands
import qualified Dag.Eff                   as SK         -- mutating stack commands
import           Control.Monad             (forM_)
import           Control.Monad.Loops       (whileJust_)
import           Data.Hashable
import qualified Data.HashSet              as HS
import           Dag                       (Dag, isLeaf, successors)
import           Data.Maybe                (fromMaybe)
import           Data.Proxy            

-- | Computes DFS for a graph which does not need to be dag, any graph will do, 
-- to define graph we only need 'adjacency :: v -> Maybe [v]'. 
-- 'adjacency' returning Nothing indicated that the argument 'v' was not in the graph. 
-- 
-- This approach nicely discloses effects used by the implemenation  
-- and nicely decouples the logic that describes the effects from the interpreters (run methods).
dfs :: forall v eff . 
      (Member  (Error String) eff                  -- error effect if adjacency returns Nothing
      , Member (ST.MutatingStackEffect v) eff      -- in-place mutating stack effect
      , Member (DS.MutatingDataStoreEffect v) eff  -- in-place mutating storage effect
      , Show v)  => 
      (v -> Maybe [v]) ->                          -- function defining adjacency (defines graph)
      v ->                                         -- root vertex 
      Eff eff [v]                                  -- list of traversed vertices wrapped in effect monad

dfs adjacency root = do 
        skH <- SK.createStack                     -- handle to new stack
        dsH <- DS.createStore                     -- handle to new datastore 
        SK.stackPush skH root
        whileJust_ (SK.stackPop skH)              -- loop until stack is empty (returns Nothing)
            (\vert ->  do
              visited <- DS.inStore dsH vert
              if visited 
              then pure ()                         -- already visisted nothing to do
              else do
                DS.addToStore dsH vert             -- mark vertex as visited placing it in datastore
                mvs <- pure . adjacency $ vert     -- get adjacent vertices
                case mvs of 
                  Nothing -> throwError $ "vertex not in graph " ++ show vert
                  Just vs -> do 
                   forM_ vs (SK.stackPush skH)     -- push all adjacent vertices on the stack
             )
        DS.getSequentialStoreContent dsH           -- return all elements in datastore in order they
                                                   -- were visited

-- | interprets all dsf effects (ST.MutatingStackEffect, DS.MutatingDataStoreEffect, Error) 
-- in the IO sin-bin 
runDsfEffIO :: (Eq v, Show v, Hashable v) => 
          Proxy v -> 
          Eff '[ST.MutatingStackEffect v, DS.MutatingDataStoreEffect v, Error String, IO] x -> 
          IO ( Either String x )
runDsfEffIO _ = runM . runError . SK.runMutatingStore . DS.runMutatingStack

-- | convenience method to just run in IO
dfsIO :: forall v . (Eq v, Show v, Hashable v) => (v -> Maybe [v]) -> v -> IO (Either String [v])
dfsIO adjacency root = do 
     runDsfEffIO (Proxy :: Proxy v) (dfs adjacency root) 

-- | DFS version of leaf calcuation to demonstrate property testing
-- see LeavesVsDfsSpec, alternative approaches to the same graph computation
-- lead to good property tests
dagLeavesUsingDfsIO :: (Dag g v e, Eq v, Hashable v, Show v) => g -> v -> IO (Maybe (HS.HashSet v))
dagLeavesUsingDfsIO g v = do 
         eitherList <- dfsIO (successors g) v
         case eitherList of 
             Left _ -> pure Nothing
             Right list -> pure $ Just . HS.fromList $ filter (fromMaybe False . isLeaf g) list
