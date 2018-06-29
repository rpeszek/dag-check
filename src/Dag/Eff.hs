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

-----------------------------------------------------
-- |
-- Module      : Dag.Eff
-- Description : Effects used by imparative graph calcuations
-----------------------------------------------------
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
-- send $ head . stack <$> readMVar stackMvar

runMutatingStackIO :: 
          MVar (Stack v) -> 
          Eff '[MutatingStackEffect v, IO] x -> 
          IO x
runMutatingStackIO stackMVar = runM . runMutatingStack stackMVar


--------------------------------------------------------------
-- MutatingStoreEffect in-place mutating stHashSet usefull for
-- marking visited nodes 
--------------------------------------------------------------

data Store a = Store {
  stHashSet :: HS.HashSet a,
  stList :: [a]
}

stAdd_ :: (Hashable a, Eq a) => a -> Store a -> Store a
stAdd_ a st = st{ stHashSet = HS.insert a $ stHashSet st, stList = a : stList st }
 
newStore :: IO (MVar (Store a))
newStore = newMVar (Store HS.empty [])

data MutatingStoreEffect a e where
    AddToStore :: a -> MutatingStoreEffect a ()
    InStore :: a -> MutatingStoreEffect a Bool
    StoreContent :: MutatingStoreEffect a [a]

addToStore :: (Member (MutatingStoreEffect a) m) => a -> Eff m ()
addToStore = send . AddToStore

inStore :: (Member (MutatingStoreEffect a) m) => a -> Eff m Bool
inStore = send . InStore

storeContent :: (Member (MutatingStoreEffect a) m) => Eff m [a]
storeContent = send StoreContent
 
runMutatingStore :: forall m a x . (Member IO m, Eq a, Hashable a) => 
                    MVar (Store a) -> Eff (MutatingStoreEffect a : m) x -> Eff m x
runMutatingStore storeMvar = interpret eval where
  eval :: (Member IO n) => MutatingStoreEffect a e -> Eff n e
  eval (AddToStore a) = send $ modifyMVar storeMvar $
    \ st -> pure (a `stAdd_` st, () )
  eval (InStore a) = send $ (HS.member a) . stHashSet <$> readMVar storeMvar
  eval StoreContent  = send $ reverse . stList <$> readMVar storeMvar

runMutatingStoreIO :: (Eq v, Hashable v) => 
          MVar (Store v) -> 
          Eff '[MutatingStoreEffect v, IO] x -> 
          IO x
runMutatingStoreIO storeMVar = runM . runMutatingStore storeMVar
