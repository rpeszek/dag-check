{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module ArbitraryDagSanitySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import ArbitraryDag

spec :: Spec
spec = do
  describe "Arbitrary Dag" $ do
    it "is a Dag" $ property $ verify
       where verify :: SimpleSequentialDag -> Bool 
             verify (ArbitrarySimpleDag (SampleDiGraph sortedVs edges)) = 
                all (\e -> fst e < snd e) edges

main :: IO ()
main = hspec spec
