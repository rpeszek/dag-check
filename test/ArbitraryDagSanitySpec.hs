module ArbitraryDagSanitySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import ArbitraryDag
import Data.List (any)

spec :: Spec
spec = do
  describe "Arbitrary Dag" $ do
    it "is a Dag" $ property $ verify
       where verify :: SimpleSequentialDag -> Bool 
             verify (ArbitrarySimpleDag (SampleDirGraph sortedVs edges)) = 
                all (\e -> fst e < snd e) edges

main :: IO ()
main = hspec spec
