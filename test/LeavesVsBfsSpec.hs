{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module LeavesVsBfsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import ArbitraryDag
import Protolude (head)
import Prelude hiding (head)
import qualified Dag.Leaves as L
import Dag.Eff.BFS (dagLeavesUsingBfsIO)

spec :: Spec
spec = do
  describe "Leaves vs BTF" $ do     
    it "topological sort approach is the same as BFS" $ property $ verify
        where verify :: SimpleSequentialDag -> Expectation 
              verify graph@(ArbitrarySimpleDag (SampleDiGraph sortedVs _)) = 
                 let mvert = head sortedVs
                 in case mvert of 
                     Nothing -> shouldBe True True
                     Just vert -> 
                         let mleaves1 = L.leaves graph vert
                             mleaves2 = dagLeavesUsingBfsIO graph vert
                         in mleaves2 `shouldReturn` mleaves1

main :: IO ()
main = hspec spec
