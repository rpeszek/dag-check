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
-- Standard imperative implemenation of DFS for arbitrary graph 
-- that spells out in-place mutations as effects
----------------------------------------------------------------------------
module Dag.Eff.DFS where
  
import           Control.Concurrent.MVar   (MVar)
import           Control.Monad.Freer       (Eff, Member, runM)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Dag.Eff
import           Control.Monad             (forM_)
import           Data.Hashable
import qualified Data.HashSet              as HS
import           Dag                       (Dag, isLeaf, successors)
import           Data.Maybe                (fromMaybe)

-- | does not need to be dag, any graph will do, only needs 'successors'
-- dfs type signature shows effects used in the calculation
dfs :: forall v eff . 
      (Member (Error String) eff
      , Member (MutatingStackEffect v) eff
      , Member (MutatingStoreEffect v) eff
      , Show v)  => 
      (v -> Maybe [v]) -> v -> Eff eff [v]
dfs successors root = stackPush root >> dfsRec [] where
     dfsRec :: [v] -> Eff eff [v]
     dfsRec acc = do
        mvert <- stackPop :: Eff eff (Maybe v)
        case mvert of
           Nothing -> pure acc
           Just vert ->  do
              visited <- inStore vert
              if visited 
              then dfsRec acc
              else do
                addToStore vert
                mvs <- pure . successors $ vert
                case mvs of 
                  Nothing -> throwError $ "vertex not in graph " ++ show vert
                  Just vs -> do 
                   forM_ vs stackPush
                   dfsRec (vert : acc)

-- | runs all dsf effects (MutatingStackEffect, MutatingStoreEffect, Error) in the IO sin bin 
runDsfEffIO :: (Eq v, Show v, Hashable v) => 
          MVar (Stack v) -> 
          MVar (Store v) ->  
          Eff '[MutatingStackEffect v, MutatingStoreEffect v, Error String, IO] x -> 
          IO ( Either String x )
runDsfEffIO stackMVar storeMVar = runM . runError . runMutatingStore storeMVar . runMutatingStack stackMVar

-- | convenience method to just run in IO
runDfsIO :: forall v . (Eq v, Show v, Hashable v) => (v -> Maybe [v]) -> v -> IO (Either String [v])
runDfsIO successors root = do 
     stackMVar <- newStack :: IO (MVar (Stack v))
     storeMVar <- newStore
     runDsfEffIO stackMVar storeMVar (dfs successors root) 

-- | DFS version of leaf calcuation
-- see LeavesVsDfsSpec, alternative approaches to the same graph computation
-- lead to good property tests
dagLeavesUsingDfs :: (Dag g v e, Eq v, Hashable v, Show v) => g -> v -> IO (Maybe (HS.HashSet v))
dagLeavesUsingDfs g v = do 
         eitherList <- runDfsIO (successors g) v
         case eitherList of 
             Left _ -> pure Nothing
             Right list -> pure $ Just . HS.fromList $ filter (fromMaybe False . isLeaf g) list
