module BCparallel where

import BCsequential2
import BasicType

import qualified Data.Map.Strict as Map
import System.Environment
-- import Data.Maybe
import Control.Parallel.Strategies

bcSolverPar :: Graph -> Map.Map Int Double
bcSolverPar g = foldl (Map.unionWith (+)) Map.empty bcMapList
    where
      bcMapList = parMap rpar singleSolver nodelist
      singleSolver :: Int -> Map.Map Int Double --solve Bc from one node
      singleSolver = calculatePerNode g iniListPred iniListSigma iniListIntDouble
      nodelist :: [Int]
      nodelist = Map.keys g
      iniListPred :: [(Int, [Int])]
      iniListPred = map (\x -> (x,[])) nodelist
      iniListSigma :: [(Int, Int)]
      iniListSigma = map (\x -> (x,0)) nodelist
      iniListIntDouble :: [(Int, Double)]
      iniListIntDouble = map (\x -> (x,0.0)) nodelist

      