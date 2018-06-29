{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Dag.Eff.BFS
-- Description :  Standard (texbook) imperative implemenation of graph BFS 
--                with effectful programming approach using in-place mutations as effects
--                and freer-simple effects library
--                This approach is almost identical to 'Dag.Eff.DFS' with 
--                mutating stack effect replaces with a mutating queue effect
----------------------------------------------------------------------------
module Dag.Eff.BFS where
  
import           Control.Concurrent.MVar   (MVar)
import           Control.Monad.Freer       (Eff, Member, runM)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Dag.Eff                   (MutatingQueueEffect, MutatingDataStoreEffect)
import qualified Dag.Eff                   as MDS        -- mutating data store commands
import qualified Dag.Eff                   as MQUE       -- mutating queue commands
import           Control.Monad             (forM_)
import           Control.Monad.Loops       (whileJust_)
import           Data.Hashable
import qualified Data.HashSet              as HS
import           Dag                       (Dag, isLeaf, successors)
import           Data.Maybe                (fromMaybe)

-- | Computes BFS for a graph which does not need to be dag, any graph will do, 
-- to define graph we only need 'adjacency :: v -> Maybe [v]'. 'adjacency' returning 
-- Nothing means that the argument 'v' was not in the graph. 
--
-- Please see 'Dag.Eff.DFS' documentation because this is almost identical 
-- the only difference is use of MutatingQueueEffect instead of MutatingStackEffect
bfs :: forall v eff . 
      (Member (Error String) eff                  
      , Member (MutatingQueueEffect v) eff        
      , Member (MutatingDataStoreEffect v) eff    
      , Show v)  => 
      (v -> Maybe [v]) ->                         
      v ->                                        
      Eff eff [v]  
                               
bfs adjacency root = MQUE.enqueue root >> bfsLoop >> MDS.getSequentialStoreContent where
     bfsLoop :: Eff eff ()     
     bfsLoop = do 
        whileJust_ (MQUE.dequeue)                 -- loop until stack is empty (returns nothing)
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
                   forM_ vs MQUE.enqueue          -- push all adjacent vertices on the queue
             )

-- | runs all bsf effects (MutatingQueueEffect, MutatingDataStoreEffect, Error) in the IO sin-bin 
runBsfEffIO :: (Eq v, Show v, Hashable v) => 
          MVar (MQUE.Queue v) -> 
          MVar (MDS.DataStore v) ->  
          Eff '[MutatingQueueEffect v, MutatingDataStoreEffect v, Error String, IO] x -> 
          IO ( Either String x )
runBsfEffIO queueMVar storeMVar = runM . runError . MQUE.runMutatingStore storeMVar . MQUE.runMutatingQueue queueMVar

-- | convenience method to just run in IO
bfsIO :: forall v . (Eq v, Show v, Hashable v) => (v -> Maybe [v]) -> v -> IO (Either String [v])
bfsIO adjacency root = do 
     stackMVar <- MQUE.newQueue :: IO (MVar (MQUE.Queue v))
     storeMVar <- MDS.newStore
     runBsfEffIO stackMVar storeMVar (bfs adjacency root) 

-- | BFS version of leaf calcuation to demonstrate property testing
-- see LeavesVsBfsSpec, alternative approaches to the same graph computation
-- lead to good property tests
dagLeavesUsingBfsIO :: (Dag g v e, Eq v, Hashable v, Show v) => g -> v -> IO (Maybe (HS.HashSet v))
dagLeavesUsingBfsIO g v = do 
         eitherList <- bfsIO (successors g) v
         case eitherList of 
             Left _ -> pure Nothing
             Right list -> pure $ Just . HS.fromList $ filter (fromMaybe False . isLeaf g) list
