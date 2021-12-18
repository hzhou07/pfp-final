module BCsequential where

import BasicType
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

sg :: Graph
sg = sampleG

shortestPathMap :: Map.Map Int [Int] -> Map.Map (Int, Int) Int
shortestPathMap g = Map.fromList [((s,e),shortestPath g s e)|s <- Map.keys g, e <- Map.keys g, (/=) s e]

shortestPath :: Graph -> Int -> Int -> Int -- graph start end -> shortestdistance
shortestPath g s e = bfs e g (Set.fromList [s]) [] 0


bfs:: Int -> Graph -> Set.Set Int -> [Int] -> Int -> Int
bfs target g frontier explored depth
    | Set.member target frontier = depth
    | otherwise = bfs target g newFrontier newExplored (depth+1)
      where
        newFrontier = Set.fromList (concatMap findchild (Set.toList frontier))
        newExplored = explored ++ Set.toList frontier
        findchild :: Int -> [Int]
        findchild i
          | Map.lookup i g == Nothing = error "invalid key for lookup"
          | otherwise = filter (\neighbor-> notElem neighbor explored) adjList
            where (Just adjList) = Map.lookup i g
          

           

{-
singlePairSovler g s e

BCsolverSequential :: Graph -> [(Int, Int)]
BCsolverSequential g = [ singlePairSovler g s e | ]
-}