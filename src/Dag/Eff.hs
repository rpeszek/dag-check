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

------------------------------------------------------------------
-- |
-- Module      : Dag.Eff
-- Description : Effects (DSLs and interpreters) used by imparative 
--               graph calcuations.
------------------------------------------------------------------
module Dag.Eff where
  
import           Control.Concurrent.MVar   (MVar, modifyMVar, newMVar, readMVar)
import           Control.Monad.Freer       (Eff, Member, interpret, send, runM)
import           Data.List                 (uncons)
import qualified Data.HashSet              as HS
import           Data.Hashable


--------------------------------------------------------------
-- MutatingStackEffect in-place mutating Stack / FIFO storage 
--------------------------------------------------------------

data Stack a = Stack {
  stack :: [a]
}

newStack :: IO (MVar (Stack a))
newStack = newMVar (Stack [])

data MutatingStackEffect a e where
    StackPush :: a -> MutatingStackEffect a ()
    StackPop :: MutatingStackEffect a (Maybe a)

stackPush :: (Member (MutatingStackEffect a) m) => a -> Eff m ()
stackPush = send . StackPush

stackPop :: (Member (MutatingStackEffect a) m) => Eff m (Maybe a)
stackPop = send StackPop

-- | Note that this interpreter accepts 'MVar (Stack a)' as input allowing 
-- for easy sharing of the stack between interpreted programs.  
-- This seems not very safe but I am not concerning myself too much about it 
-- in this conceptual code.
-- 
-- The same design is used for other interpreters in this package.
runMutatingStack :: forall m a x . (Member IO m) => 
                    MVar (Stack a) -> Eff (MutatingStackEffect a : m) x -> Eff m x
runMutatingStack stackMvar = interpret eval where
  eval :: (Member IO n) => MutatingStackEffect a e -> Eff n e
  eval (StackPush a) = send $ modifyMVar stackMvar $
    \ st -> pure (st{ stack = a : stack st }, () )
  eval StackPop = send $ modifyMVar stackMvar $
    \ st -> 
          let muncons = uncons . stack $ st
          in case muncons of
             Nothing -> pure (st{ stack = [] }, Nothing)
             Just (x, xs) -> pure (st{ stack = xs }, Just x)

runMutatingStackIO :: 
          MVar (Stack v) -> 
          Eff '[MutatingStackEffect v, IO] x -> 
          IO x
runMutatingStackIO stackMVar = runM . runMutatingStack stackMVar


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
 
newStore :: IO (MVar (DataStore a))
newStore = newMVar (DataStore HS.empty [])

data MutatingDataStoreEffect a e where
    AddToStore :: a -> MutatingDataStoreEffect a ()
    InStore :: a -> MutatingDataStoreEffect a Bool
    GetDataStoreSequentialContent :: MutatingDataStoreEffect a [a]

addToStore :: (Member (MutatingDataStoreEffect a) m) => a -> Eff m ()
addToStore = send . AddToStore

inStore :: (Member (MutatingDataStoreEffect a) m) => a -> Eff m Bool
inStore = send . InStore

getSequentialStoreContent :: (Member (MutatingDataStoreEffect a) m) => Eff m [a]
getSequentialStoreContent = send GetDataStoreSequentialContent
 
runMutatingStore :: forall m a x . (Member IO m, Eq a, Hashable a) => 
                    MVar (DataStore a) -> Eff (MutatingDataStoreEffect a : m) x -> Eff m x
runMutatingStore storeMvar = interpret eval where
  eval :: (Member IO n) => MutatingDataStoreEffect a e -> Eff n e
  eval (AddToStore a) = send $ modifyMVar storeMvar $
    \ st -> pure (a `dstAdd_` st, () )
  eval (InStore a) = send $ (HS.member a) . stHashSet <$> readMVar storeMvar
  eval GetDataStoreSequentialContent  = send $ reverse . stList <$> readMVar storeMvar

runMutatingStoreIO :: (Eq v, Hashable v) => 
          MVar (DataStore v) -> 
          Eff '[MutatingDataStoreEffect v, IO] x -> 
          IO x
runMutatingStoreIO storeMVar = runM . runMutatingStore storeMVar


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


newQueue :: IO (MVar (Queue a))
newQueue = newMVar (emptyQueue)

data MutatingQueueEffect a e where
    Enqueue :: a -> MutatingQueueEffect a ()
    Dequeue :: MutatingQueueEffect a (Maybe a)

enqueue :: (Member (MutatingQueueEffect a) m) => a -> Eff m ()
enqueue = send . Enqueue

dequeue :: (Member (MutatingQueueEffect a) m) => Eff m (Maybe a)
dequeue = send Dequeue

runMutatingQueue :: forall m a x . (Member IO m) => 
                    MVar (Queue a) -> Eff (MutatingQueueEffect a : m) x -> Eff m x
runMutatingQueue queueMvar = interpret eval where
  eval :: (Member IO n) => MutatingQueueEffect a e -> Eff n e
  eval (Enqueue a) = send $ modifyMVar queueMvar $
    \ q -> pure (a `enqueue_` q, () )
  eval Dequeue = send $ modifyMVar queueMvar $
    \ q -> pure . dequeue_ $ q
