{-
Arbitrary Simple Directed Acyclic graph that has positive likelihood to generate 
every possible DAG topology
-}

{-# LANGUAGE 
   MultiParamTypeClasses
  , ScopedTypeVariables
  , FlexibleInstances
  , GeneralizedNewtypeDeriving 
#-}

module ArbitraryDag where
import Test.QuickCheck
import Data.List
import System.Random (Random)
import Control.Monad (replicateM)
import Data.Maybe (catMaybes)
import Data.Bool (bool)

{- Helper data types used for Arbitrary generation -}

{-| Represents general directed graph -}
data SampleDirGraph v e = SampleDirGraph {
   sortedVerts :: [v]
  , edges :: [e]
} deriving Show

empty :: SampleDirGraph v e 
empty = SampleDirGraph [] []

singleton ::  v -> SampleDirGraph v e
singleton vx = SampleDirGraph [vx] [] 

disconnected ::  [v] -> SampleDirGraph v e
disconnected vx = SampleDirGraph vx [] 

{-| Represents randomly generated Simple DAG -}
data ArbitrarySimpleDag v e = ArbitrarySimpleDag (SampleDirGraph v e) deriving Show

{- Convience types (Seqential is defined below)-}
type SimpleSequentialDag = ArbitrarySimpleDag Seqential (Seqential, Seqential)    
type SimpleIntDag = ArbitrarySimpleDag Int (Int, Int)

instance (GenSimpleEdges v e, GenVertices v, Eq e) => Arbitrary (ArbitrarySimpleDag v e) where
    arbitrary = genSimpleDag


{- DAGs are generated by implementing 2 interfaces GenVertices and GenSimpleEdges -}

{-| First interface needed for DAG generation -}
class GenVertices a where
  genVertices :: Int -> Gen [a]

instance GenVertices Int where
  genVertices = vector

{-| Second interface needed for DAG generation -}
class GenSimpleEdges v e where
   genEdges :: [v] -> Gen [e]

instance GenSimpleEdges Int (Int, Int) where
  genEdges = genEdgesDefault


{-| Helper interface that simplifies implemeting GenSimpleEdges -}
class GenEdge v e where
  genEdge :: v -> v -> Gen e

instance GenEdge a (a,a) where
  genEdge x y = return (x,y)

{-| For default impementation of GenSimpleEdges -}
genEdgesDefault :: forall v e. (Eq e, GenEdge v e) => [v] -> Gen [e]
genEdgesDefault vertices = 
      let vcount = length vertices
          eCountMax = (vcount - 1) * vcount `quot` 2 --max possible number of edges in simple dag             
          -- defines a (possibly redunant) edge for topologically sorted vertices [v]
          newEdge :: [v] -> Gen (Maybe e)
          newEdge vertices = case length vertices of
           n
            | n < 2 -> return Nothing
            | n == 2 -> arbitrary >>= bool (return Nothing) (Just <$> genEdge (vertices !! 0) (vertices !! 1)) 
            | otherwise -> do
               inx <- choose (0, length vertices - 2) -- skip last element to give it room for a potential edge
               to <- elements $ drop (inx + 1) vertices
               fmap Just $ genEdge (vertices !! inx) to
      in fmap (catMaybes . nub) . replicateM eCountMax $ newEdge vertices


{- Helper / alternative methods for DAG generation -}

genSizedSimpleDag :: (GenSimpleEdges v e, GenVertices v, Eq e) => Int -> Gen (ArbitrarySimpleDag v e)
genSizedSimpleDag vcount = do
  case vcount of
     0 -> return $ ArbitrarySimpleDag empty
     1 -> fmap (ArbitrarySimpleDag . disconnected) (genVertices 1)  
     n -> 
        do 
          vertices <- genVertices vcount
          edges <- genEdges vertices
          return $ ArbitrarySimpleDag $ SampleDirGraph vertices edges

genSimpleDag :: (GenSimpleEdges v e, GenVertices v, Eq e) => Gen (ArbitrarySimpleDag v e)
genSimpleDag = arbitrarySizedNatural >>= genSizedSimpleDag
    
{-| Seqential type used for DAGs that are nicely sorted -}
newtype Seqential = Seqential Int deriving (Num, Eq, Ord)

instance Show Seqential where
  show (Seqential i) = show i

instance Arbitrary Seqential where
   arbitrary = Seqential . abs <$> (arbitrary :: Gen Int)

instance GenVertices Seqential where
  genVertices size = return $ map Seqential [0..size]

instance GenSimpleEdges Seqential (Seqential, Seqential) where
  genEdges = fmap sort . genEdgesDefault


{- test methods -}
test :: IO (SimpleIntDag)
test = generate arbitrary
-- 
testP :: IO (SimpleSequentialDag)
testP = generate arbitrary


{- Other: mapping between vertices and edges -}
class DiEdge v e where
  resolveEdge :: e -> (v,v)

instance DiEdge a (a,a) where
  resolveEdge = id
