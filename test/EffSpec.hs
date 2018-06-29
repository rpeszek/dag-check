
module EffSpec where
  
import           Dag.Eff 
import           Control.Concurrent.MVar   (MVar, readMVar)
import           Test.Hspec
import qualified Data.HashSet              as HS

spec :: Spec
spec = describe "StackEff Tests" $ do
  it "test stackPush" $ do
    mVar <- newStack :: IO (MVar (Stack Int))
    runMutatingStackIO mVar $ do
                                    stackPush (1 :: Int)
                                    stackPush (2 :: Int)
    content <- readMVar mVar
    stack content `shouldBe` [2,1]

  it "test pull" $ do
    mVar <- newStack :: IO (MVar (Stack Int))
    got <- runMutatingStackIO mVar $ (do
                                    stackPush (1 :: Int)
                                    stackPop ) :: IO (Maybe Int) 
    content <- readMVar mVar
    (got, stack content) `shouldBe` (Just 1, [])
  describe "StoreEff Tests" $ do
    it "test addToStore" $ do
       mVar <- newStore :: IO (MVar (Store Int))
       got <- runMutatingStoreIO mVar $ do
                  addToStore (1 :: Int)
                  addToStore (2 :: Int)
                  storeContent
       store <- readMVar mVar
       (got, stHashSet store) `shouldBe` ([1,2] :: [Int], HS.fromList [2,1])
    it "test inStore" $ do
       mVar <- newStore :: IO (MVar (Store Int))
       got <- runMutatingStoreIO mVar $ do
                  addToStore (1 :: Int)
                  addToStore (2 :: Int)
                  x <- inStore (2 :: Int)
                  y <- inStore (4 :: Int)
                  pure (x, y)
       got `shouldBe` (True, False)
