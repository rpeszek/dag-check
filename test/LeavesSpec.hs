module LeavesSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import ArbitraryDag
import Protolude (head)
import Prelude hiding (head)
import qualified Data.HashSet as HS
import qualified Dag.Leaves as L
import Dag (isLeaf)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

spec :: Spec
spec = do
  describe "Leaves" $ do
    it "leaves have no successors and exist" $ property $ verify
       where verify :: SimpleSequentialDag -> Bool 
             verify graph@(ArbitrarySimpleDag (SampleDirGraph sortedVs _)) = 
                let mvert = head sortedVs
                in case mvert of 
                    Nothing -> True
                    Just vert -> 
                        let mleaves = L.leaves graph vert
                        in case mleaves of
                          Nothing -> False -- ^ This condition would mean that first vertex is not in the graph
                          Just lvs -> (not . HS.null $ lvs) && 
                                      (all (fromMaybe False . isLeaf graph) $ HS.toList lvs) 

main :: IO ()
main = hspec spec

debug :: Show a => String -> a -> a
debug msg a = unsafePerformIO $ do 
    putStrLn $ msg
    return a 
