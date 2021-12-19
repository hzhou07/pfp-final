module BasicType where

import qualified Data.Map.Strict as Map
{-
Sample graph presented by adjacency list
{
1: 2 
2: 1,3
3: 2,4
4: 3
}
-}


type Graph = Map.Map Int [Int]

sampleG :: Graph
sampleG = Map.fromList [(1,[2]),(2,[1,3]),(3,[2,4]),(4,[3])]