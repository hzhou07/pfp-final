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


calculateSigmaAndSoOn :: Graph -> Set.Set Int -> [Int] -> Set.Set Int -> Map.Map (Int, [Int]) -> Map.Map (Int, Int) -> Int -> (Map.Map (Int, [Int]), Map.Map (Int, Int), [Int]) 
calculateSigmaAndSoOn g frontier s explored pred sigma depth
    | Set.null frontier = (pred, sigma, s)
    | otherwise = calculateSigmaAndSoOn g newFrontier newS newExplored newPred newSigma depth+1
    where 
        newFrontier = Set.fromList (concatMap findchild (Set.toList frontier))
        newS = s ++ Set.toList frontier
        newExplored = Set.union explored frontier
        (newPred, newSigma ) = updatePredSigma pred sigma (Set.toList frontier)    
        findchild :: Int -> [Int]
        findchild i
          | Map.lookup i g == Nothing = error "invalid key for lookup"
          | otherwise = filter (\neighbor-> Set.notMember neighbor explored) adjList
            where (Just adjList) = Map.lookup i g
        updatePredSigma :: [Int] -> Map.Map (Int, [Int]) -> Map.Map (Int, Int) -> (Map.Map (Int, [Int]), Map.Map (Int, Int))
        updatePredSigma p1 s1 [] = (p1, s1)
        updatePredSigma p1 s1 (v:vs) = updatePredSigma p1New s1New vs
            where
              (findchild v) 
              p1New =
         

accumulateCB :: Map.Map (Int, Double) -> Map.Map (Int, Double) -> Map.Map (Int, [Int]) -> Map.Map (Int, Int) -> [Int]
accumulateCB cb delta pred sigma s

calculatePerNode :: Int -> Graph -> Map.Map (Int, Double)
calculatePerNode node g
    
           

{-
singlePairSovler g s e

BCsolverSequential :: Graph -> [(Int, Int)]
BCsolverSequential g = [ singlePairSovler g s e | ]
-}
