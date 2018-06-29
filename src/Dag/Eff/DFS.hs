{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeOperators    #-}
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
import           Dag.Eff
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
-- Note that the type signature shows effects used in the calculation, this method 
-- is effect polymorphic and can run in any monad that has specified effects. 
dfs :: forall v eff . 
      (Member (Error String) eff           -- error effect if ajacency returns Nothing
      , Member (MutatingStackEffect v) eff -- in-place mutating stack
      , Member (MutatingStoreEffect v) eff -- in-place mutating storage
      , Show v)  => 
      (v -> Maybe [v]) ->                  -- graph defining function defining adjacency
      v ->                                 -- root vertex 
      Eff eff [v]                          -- list of traversed vertices

dfs adjacency root = stackPush root >> dfsLoop >> storeContent where
     dfsLoop :: Eff eff ()     
     dfsLoop = do 
        whileJust_ (stackPop)
            (\vert ->  do
              visited <- inStore vert
              if visited 
              then pure ()
              else do
                addToStore vert
                mvs <- pure . adjacency $ vert
                case mvs of 
                  Nothing -> throwError $ "vertex not in graph " ++ show vert
                  Just vs -> do 
                   forM_ vs stackPush
             )

-- | runs all dsf effects (MutatingStackEffect, MutatingStoreEffect, Error) in the IO sin bin 
runDsfEffIO :: (Eq v, Show v, Hashable v) => 
          MVar (Stack v) -> 
          MVar (Store v) ->  
          Eff '[MutatingStackEffect v, MutatingStoreEffect v, Error String, IO] x -> 
          IO ( Either String x )
runDsfEffIO stackMVar storeMVar = runM . runError . runMutatingStore storeMVar . runMutatingStack stackMVar

-- | convenience method to just run in IO
runDfsIO :: forall v . (Eq v, Show v, Hashable v) => (v -> Maybe [v]) -> v -> IO (Either String [v])
runDfsIO adjacency root = do 
     stackMVar <- newStack :: IO (MVar (Stack v))
     storeMVar <- newStore
     runDsfEffIO stackMVar storeMVar (dfs adjacency root) 

-- | DFS version of leaf calcuation
-- see LeavesVsDfsSpec, alternative approaches to the same graph computation
-- lead to good property tests
dagLeavesUsingDfs :: (Dag g v e, Eq v, Hashable v, Show v) => g -> v -> IO (Maybe (HS.HashSet v))
dagLeavesUsingDfs g v = do 
         eitherList <- runDfsIO (successors g) v
         case eitherList of 
             Left _ -> pure Nothing
             Right list -> pure $ Just . HS.fromList $ filter (fromMaybe False . isLeaf g) list
