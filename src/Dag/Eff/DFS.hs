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
--                with effectful programming approach using in-place mutations as effects
--                and freer-simple effects library
----------------------------------------------------------------------------
module Dag.Eff.DFS where
  
import           Control.Concurrent.MVar   (MVar)
import           Control.Monad.Freer       (Eff, Member, runM)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Dag.Eff                   (MutatingStackEffect, MutatingDataStoreEffect)
import qualified Dag.Eff                   as MDS        -- mutating data store commands
import qualified Dag.Eff                   as MSTK       -- mutating stack commands
import           Control.Monad             (forM_)
import           Control.Monad.Loops       (whileJust_)
import           Data.Hashable
import qualified Data.HashSet              as HS
import           Dag                       (Dag, isLeaf, successors)
import           Data.Maybe                (fromMaybe)

-- | Computes DFS for a graph which does not need to be dag, any graph will do, 
-- to define graph we only need 'adjacency :: v -> Maybe [v]'. 'adjacency' returning 
-- Nothing means that the argument 'v' was not in the graph. 
-- 
-- This approach nicely discloses fine grain effects used by the implemenation  
-- and nicely decouples the logic that describes the effects from the interpreters (run methods).
-- It does not provide type safety over, for example, reusing a stack or a data store between 
-- programs that are implemented using MutatingStackEffect and MutatingDataStoreEffect DSLs.
-- This seems not very safe but I am not concerning myself too much about it in this conceptual code.
dfs :: forall v eff . 
      (Member (Error String) eff                  -- error effect if adjacency returns Nothing
      , Member (MutatingStackEffect v) eff        -- in-place mutating stack
      , Member (MutatingDataStoreEffect v) eff    -- in-place mutating storage
      , Show v)  => 
      (v -> Maybe [v]) ->                         -- function defining adjacency (defines graph)
      v ->                                        -- root vertex 
      Eff eff [v]                                 -- list of traversed vertices wrapped in effect monad

-- | 'getSequentialStoreContent' returns elements in MutatingDataStore
-- in the order the elements were added to it
dfs adjacency root = MSTK.stackPush root >> dfsLoop >> MDS.getSequentialStoreContent where
     dfsLoop :: Eff eff ()     
     dfsLoop = do 
        whileJust_ (MSTK.stackPop)                -- loop until stack is empty (returns nothing)
            (\vert ->  do
              visited <- MDS.inStore vert
              if visited 
              then pure ()                        -- already visisted nothing to do
              else do
                MDS.addToStore vert               -- mark vertex as visited
                mvs <- pure . adjacency $ vert    -- get adjacent vertices
                case mvs of 
                  Nothing -> throwError $ "vertex not in graph " ++ show vert
                  Just vs -> do 
                   forM_ vs MSTK.stackPush        -- push all adjacent vertices on the stack
             )

-- | runs all dsf effects (MutatingStackEffect, MutatingDataStoreEffect, Error) in the IO sin-bin 
runDsfEffIO :: (Eq v, Show v, Hashable v) => 
          MVar (MSTK.Stack v) -> 
          MVar (MDS.DataStore v) ->  
          Eff '[MutatingStackEffect v, MutatingDataStoreEffect v, Error String, IO] x -> 
          IO ( Either String x )
runDsfEffIO stackMVar storeMVar = runM . runError . MSTK.runMutatingStore storeMVar . MDS.runMutatingStack stackMVar

-- | convenience method to just run in IO
dfsIO :: forall v . (Eq v, Show v, Hashable v) => (v -> Maybe [v]) -> v -> IO (Either String [v])
dfsIO adjacency root = do 
     stackMVar <- MSTK.newStack :: IO (MVar (MSTK.Stack v))
     storeMVar <- MDS.newStore
     runDsfEffIO stackMVar storeMVar (dfs adjacency root) 

-- | DFS version of leaf calcuation to demonstrate property testing
-- see LeavesVsDfsSpec, alternative approaches to the same graph computation
-- lead to good property tests
dagLeavesUsingDfsIO :: (Dag g v e, Eq v, Hashable v, Show v) => g -> v -> IO (Maybe (HS.HashSet v))
dagLeavesUsingDfsIO g v = do 
         eitherList <- dfsIO (successors g) v
         case eitherList of 
             Left _ -> pure Nothing
             Right list -> pure $ Just . HS.fromList $ filter (fromMaybe False . isLeaf g) list
