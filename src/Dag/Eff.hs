{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

------------------------------------------------------------------
-- |
-- Module      : Dag.Eff
-- Description : Effects (DSLs and interpreters) used by imparative 
--               graph calcuations.
--
-- TODOs       : This module exports too much should be reorgenized and possibly
--               repackaged to hide some of the implemenation details
--               (like DataStore, Stack, and Queue and other things 
--               not exposed by DSLs)
------------------------------------------------------------------
module Dag.Eff where
  
import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar, readMVar)
import           Control.Monad.Freer       (Eff, Member, interpret, send, runM)
import           Data.List                 (uncons)
import qualified Data.HashSet              as HS
import           Data.Hashable
import           Data.Proxy            


--------------------------------------------------------------
-- MutatingStackEffect in-place mutating Stack / FIFO storage 
--------------------------------------------------------------

data Stack a = Stack {
  stack :: [a]
}

type StackHandle a = MVar (Stack a)

data MutatingStackEffect a e where
    CreateStack :: MutatingStackEffect a (StackHandle a)
    StackPush :: StackHandle a -> a -> MutatingStackEffect a ()
    StackPop :: StackHandle a -> MutatingStackEffect a (Maybe a)

createStack :: (Member (MutatingStackEffect a) m) => Eff m (StackHandle a)
createStack = send CreateStack

stackPush :: (Member (MutatingStackEffect a) m) => StackHandle a -> a -> Eff m ()
stackPush stH = send . StackPush stH

stackPop :: (Member (MutatingStackEffect a) m) =>StackHandle a ->  Eff m (Maybe a)
stackPop = send . StackPop

runMutatingStack :: forall m a x . (Member IO m) => 
                    Eff (MutatingStackEffect a : m) x -> Eff m x
runMutatingStack = interpret eval where
  eval :: (Member IO n) => MutatingStackEffect a e -> Eff n e
  eval CreateStack =  send $ newMVar (Stack [])
  eval (StackPush stackH a) = send $ modifyMVar stackH $
    \ st -> pure (st{ stack = a : stack st }, () )
  eval (StackPop stackH) = send $ modifyMVar stackH $
    \ st -> 
          let muncons = uncons . stack $ st
          in case muncons of
             Nothing -> pure (st{ stack = [] }, Nothing)
             Just (x, xs) -> pure (st{ stack = xs }, Just x)

runMutatingStackIO ::
          Proxy v ->  
          Eff '[MutatingStackEffect v, IO] x -> 
          IO x
runMutatingStackIO _ = runM . runMutatingStack


--------------------------------------------------------------
-- MutatingDataStoreEffect in-place mutating Data Store usefull for
-- marking visited nodes 
-- it remembers in which order elements have been added to it
--------------------------------------------------------------

data DataStore a = DataStore {
  stHashSet :: HS.HashSet a,
  stList :: [a]
}

dstAdd_ :: (Hashable a, Eq a) => a -> DataStore a -> DataStore a
dstAdd_ a st = st{ stHashSet = HS.insert a $ stHashSet st, stList = a : stList st }

type DataStoreHandle a = MVar (DataStore a)
 
data MutatingDataStoreEffect a e where
    CreateStore :: MutatingDataStoreEffect a (DataStoreHandle a)
    AddToStore :: DataStoreHandle a -> a -> MutatingDataStoreEffect a ()
    InStore :: DataStoreHandle a -> a -> MutatingDataStoreEffect a Bool
    GetDataStoreSequentialContent :: DataStoreHandle a -> MutatingDataStoreEffect a [a]

createStore :: (Member (MutatingDataStoreEffect a) m) => Eff m (DataStoreHandle a)
createStore = send CreateStore

addToStore :: (Member (MutatingDataStoreEffect a) m) => DataStoreHandle a -> a -> Eff m ()
addToStore stH = send . AddToStore stH

inStore :: (Member (MutatingDataStoreEffect a) m) => DataStoreHandle a -> a -> Eff m Bool
inStore stH = send . InStore stH

getSequentialStoreContent :: (Member (MutatingDataStoreEffect a) m) => DataStoreHandle a -> Eff m [a]
getSequentialStoreContent = send . GetDataStoreSequentialContent
 
runMutatingStore :: forall m a x . (Member IO m, Eq a, Hashable a) => 
                  Eff (MutatingDataStoreEffect a : m) x -> Eff m x
runMutatingStore = interpret eval where
  eval :: (Member IO n) => MutatingDataStoreEffect a e -> Eff n e
  eval CreateStore = send $ newMVar (DataStore HS.empty [])
  eval (AddToStore stH a) = send $ modifyMVar stH $
    \ st -> pure (a `dstAdd_` st, () )
  eval (InStore stH a) = send $ (HS.member a) . stHashSet <$> readMVar stH
  eval (GetDataStoreSequentialContent stH) = send $ reverse . stList <$> readMVar stH

runMutatingStoreIO :: (Eq v, Hashable v) => 
          Proxy v ->  
          Eff '[MutatingDataStoreEffect v, IO] x -> 
          IO x
runMutatingStoreIO _ = runM . runMutatingStore 


--------------------------------------------------------------
-- MutatingQueueEffect in-place mutating Queue / FIFO effect
--------------------------------------------------------------
data Queue a = Queue { 
  inbox :: [a], 
  outbox :: [a] 
} deriving (Eq, Show)

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue_ :: a -> Queue a -> Queue a
enqueue_ e (Queue inb out) = Queue (e:inb) out

dequeue_ :: Queue a -> (Queue a, Maybe a)
dequeue_ q = 
  case top of
    Nothing   -> (emptyQueue, top)
    Just _ -> (poppedQueue, top)
    where
      (q', top) = fixqueue_ q
      poppedQueue = Queue (inbox q') (tail $ outbox q')

fixqueue_ :: Queue a -> (Queue a, Maybe a)
fixqueue_ q@(Queue [] [])    = (emptyQueue, Nothing)
fixqueue_ q@(Queue inb [])   = fixqueue_ $ Queue [] (reverse inb)
fixqueue_ q@(Queue _ outb)   = (q, Just $ head outb)

type QueueHandle a = MVar (Queue a)

data MutatingQueueEffect a e where
    CreateQueue :: MutatingQueueEffect a (QueueHandle a)
    Enqueue :: QueueHandle a -> a -> MutatingQueueEffect a ()
    Dequeue :: QueueHandle a -> MutatingQueueEffect a (Maybe a)

createQueue :: (Member (MutatingQueueEffect a) m) => Eff m (QueueHandle a)
createQueue = send CreateQueue

enqueue :: (Member (MutatingQueueEffect a) m) => QueueHandle a -> a -> Eff m ()
enqueue quH = send . Enqueue quH

dequeue :: (Member (MutatingQueueEffect a) m) => QueueHandle a -> Eff m (Maybe a)
dequeue = send . Dequeue

runMutatingQueue :: forall m a x . (Member IO m) => 
                    Eff (MutatingQueueEffect a : m) x -> Eff m x
runMutatingQueue = interpret eval where
  eval :: (Member IO n) => MutatingQueueEffect a e -> Eff n e
  eval CreateQueue = send $ newMVar emptyQueue
  eval (Enqueue quH a) = send $ modifyMVar quH $
    \ q -> pure (a `enqueue_` q, () )
  eval (Dequeue quH) = send $ modifyMVar quH $
    \ q -> pure . dequeue_ $ q

runMutatingQueueIO :: (Eq v, Hashable v) => 
          Proxy v ->  
          Eff '[MutatingQueueEffect v, IO] x -> 
          IO x
runMutatingQueueIO _ = runM . runMutatingQueue 
