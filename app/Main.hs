module Main where

import System.Exit(die)
import System.Environment(getArgs, getProgName)

import BCsequential

main :: IO ()
--main = someFunc
main = do args <- getArgs
          case args of
            [version] -> do
              case version of
                   "sequential" -> do print version
                                      print $ shortestPathMap sg
                   "parallel" -> print version
                   _ -> die $ "Usage: Choose correct version (sequential / parallel)"
            _ -> do pn <- getProgName
                    die $ "Usage: "++pn++" <version>"
