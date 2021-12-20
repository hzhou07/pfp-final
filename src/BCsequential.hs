module BCsequential where

import BasicType
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

sg :: Graph
sg = sampleG

shortestPathMap :: Map.Map Int [Int] -> Map.Map (Int, Int) Int
shortestPathMap g = Map.fromList [((s,e),shortestPath g s e)|s <- Map.keys g, e <- Map.keys g, (/=) s e]

shortestPath :: Graph -> Int -> Int -> Int
shortestPath g s e = bfs e g (Set.fromList [s]) [] 0


bigG :: Graph
bigG =  sampleG


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


calculateSigmaAndSoOn :: Graph -> Set.Set Int -> [Int] -> Set.Set Int -> Map.Map Int [Int] -> Map.Map Int Int -> (Map.Map Int [Int], Map.Map Int Int, [Int]) 
calculateSigmaAndSoOn g frontier s explored prede sigma
    | Set.null frontier = (prede, sigma, s)
    | otherwise = calculateSigmaAndSoOn g newFrontier newS newExplored newPred newSigma
    where 
        newFrontier = Set.fromList (concatMap findchild (Set.toList frontier))
        newS = s ++ Set.toList frontier
        newExplored = Set.union explored frontier
        (newPred, newSigma) = updatePredSigma prede sigma (Set.toList frontier)    
        findchild :: Int -> [Int]
        findchild i
          | Map.lookup i g == Nothing = error "invalid key for lookup"
          | otherwise = filter (\neighbor-> Set.notMember neighbor newExplored) adjList
            where (Just adjList) = Map.lookup i g
        updatePredSigma :: Map.Map Int [Int] -> Map.Map Int Int -> [Int] ->(Map.Map Int [Int], Map.Map Int Int)
        updatePredSigma p1 s1 [] = (p1, s1)
        updatePredSigma p1 s1 (v:vs) = updatePredSigma p1New s1New vs
            where
              (p1New, s1New) = updateForSingleParent (findchild v) p1 s1
              updateForSingleParent :: [Int] ->  Map.Map Int [Int] -> Map.Map Int Int -> (Map.Map Int [Int], Map.Map Int Int)
              updateForSingleParent [] p2 s2 = (p2,s2)
              updateForSingleParent (w:ws) p2 s2 = updateForSingleParent ws p2New s2New
                where
                  p2New = Map.insertWith (++) w [v] p2
                  s2New = Map.insertWith (+) w sig s2
                    where (Just sig) = Map.lookup v s2
             
         

accumulateCB :: Map.Map Int Double -> Int -> Map.Map Int Double -> Map.Map Int [Int] -> Map.Map Int Int -> [Int] ->  Map.Map Int Double
accumulateCB cb _ _ _ _ [] = cb
accumulateCB cb start delta prede sigma (w:ws) = accumulateCB cbNew start deltaNew prede sigma ws
    where
      (Just deltaW) = Map.lookup w delta
      cbNew = if (/=) w start then Map.insertWith (+) w deltaW cb else cb
      deltaNew = updateDelta predList delta
        where 
          (Just predList) = Map.lookup w prede
          updateDelta :: [Int] -> Map.Map Int Double -> Map.Map Int Double
          updateDelta [] d = d
          updateDelta (v:vs) d = updateDelta vs dNew
            where
              dNew = Map.insertWith (+) v dValue d
              dValue :: Double
              dValue = (fromIntegral sigmaV) / (fromIntegral sigmaW) * (1.0 + deltaW)::Double
              (Just sigmaV) = Map.lookup v sigma
              (Just sigmaW) = Map.lookup w sigma
              
          

calculatePerNode :: Graph -> [(Int, [Int])] -> [(Int, Int)] -> [(Int, Double)] -> Int ->Map.Map Int Double
calculatePerNode g iniListPred iniListSigma iniListIntDouble node = Map.map (/ 2.0) resultMap
    where
      resultMap = accumulateCB (Map.fromList iniListIntDouble) node (Map.fromList iniListIntDouble) prede sigma (reverse s)
      (prede,sigma,s) = calculateSigmaAndSoOn g (Set.fromList [node]) [] Set.empty (Map.fromList iniListPred) (Map.insert node 1 (Map.fromList iniListSigma))
        

bcSolver :: Graph -> Map.Map Int Double
bcSolver g = foldl (Map.unionWith (+)) Map.empty bcMapList
    where
      bcMapList::[Map.Map Int Double]
      bcMapList = map (calculatePerNode g iniListPred iniListSigma iniListIntDouble) nodelist
      nodelist :: [Int]
      nodelist = Map.keys g
      iniListPred :: [(Int, [Int])]
      iniListPred = map (\x -> (x,[])) nodelist
      iniListSigma :: [(Int, Int)]
      iniListSigma = map (\x -> (x,0)) nodelist
      iniListIntDouble :: [(Int, Double)]
      iniListIntDouble = map (\x -> (x,0.0)) nodelist
      