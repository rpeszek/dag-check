{-# LANGUAGE ScopedTypeVariables #-}

module EffSpec where
  
import           Test.Hspec
import           Test.QuickCheck
import           Dag.Eff 
import           Control.Concurrent.MVar   (MVar, readMVar)
import qualified Data.HashSet              as HS
import           Data.Proxy
import           Control.Monad             (forM_)
import           Control.Monad.Loops       (whileJust)

testStackLIFO :: [Int] -> Expectation
testStackLIFO input = do
       output <- runMutatingStackIO (Proxy :: Proxy Int) $ do
                h <- createStack
                forM_ input (stackPush h)  
                whileJust (stackPop h) pure
       (reverse input) `shouldBe` output 


testQueueFIFO :: [Int] -> Expectation
testQueueFIFO input = do
       output <- runMutatingQueueIO (Proxy :: Proxy Int) $ do
                h <- createQueue
                forM_ input (enqueue h)  
                whileJust (dequeue h) pure
       input `shouldBe` output 

testStoreFIFO :: [Int] -> Expectation
testStoreFIFO input = do
       output <- runMutatingStoreIO (Proxy :: Proxy Int) $ do
                h <- createStore
                forM_ input (addToStore h)  
                getSequentialStoreContent h
       input `shouldBe` output 

spec :: Spec
spec = describe "Stack Effect Internals" $ do
  it "test stackPush" $ do
    mVar <- runMutatingStackIO (Proxy :: Proxy Int) $ do                              
                                stH <- createStack
                                stackPush stH (1 :: Int)
                                stackPush stH 2
                                pure stH
    content <- readMVar mVar
    stack content `shouldBe` [2,1]
  it "test pop" $ do
    (mVar, got) <- runMutatingStackIO (Proxy :: Proxy Int) $ do
                                    stH <- createStack
                                    stackPush stH (1 :: Int)
                                    just1 <- stackPop stH
                                    pure (stH, just1)
    content <- readMVar mVar
    (stack content, got) `shouldBe` ([], Just 1)
  
  describe "Stack Effect Properties" $ do 
    it "lifo test" $ property $ testStackLIFO
  
  describe "Store Effect Internals" $ do
    it "test addToStore" $ do
       (mVar, got) <- runMutatingStoreIO (Proxy :: Proxy Int) $ do
                  dsH <- createStore
                  addToStore dsH (1 :: Int)
                  addToStore dsH 2
                  content <- getSequentialStoreContent dsH
                  pure (dsH, content)
       store <- readMVar mVar
       (got, stHashSet store) `shouldBe` ([1,2] :: [Int], HS.fromList [2,1])
    it "test inStore" $ do
       got <- runMutatingStoreIO (Proxy :: Proxy Int) $ do
                  dsH <- createStore
                  addToStore dsH (1 :: Int)
                  addToStore dsH (2 :: Int)
                  x <- inStore dsH (2 :: Int)
                  y <- inStore dsH (4 :: Int)
                  pure (x, y)
       got `shouldBe` (True, False)
  
  describe "Store Effect Properties" $ do
    it "fifo test" $ property $ testStoreFIFO

  describe "Queue Effect Properties" $ do
    it "fifo test" $ property $ testQueueFIFO
