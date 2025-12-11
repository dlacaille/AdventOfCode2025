module Solution where

import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type Graph = HashMap String [String]

puzzle1 :: Graph -> IO Int
puzzle1 graph =
  let countPaths :: String -> Set String -> Int
      countPaths current visited
        | current == "out" = 1
        | otherwise =
            let neighbors = HashMap.findWithDefault [] current graph
                unvisitedNeighbors = filter (`Set.notMember` visited) neighbors
                newVisited = Set.insert current visited
             in sum [countPaths neighbor newVisited | neighbor <- unvisitedNeighbors]
      result = countPaths "you" Set.empty
   in return result

puzzle2 :: Graph -> IO Int
puzzle2 graph =
  let countPaths :: String -> Set String -> Set String -> State (HashMap (String, Set String) Int) Int
      countPaths current visited required = do
        let cacheKey = (current, required)
        cache <- get
        case HashMap.lookup cacheKey cache of
          Just cached -> return cached
          Nothing -> do
            pathCount <-
              if current == "out" && Set.null required
                then return 1
                else
                  if current == "out"
                    then return 0
                    else do
                      let neighbors = HashMap.findWithDefault [] current graph
                          unvisitedNeighbors = filter (`Set.notMember` visited) neighbors
                          newVisited = Set.insert current visited
                          newRequired = Set.delete current required
                      results <- mapM (\neighbor -> countPaths neighbor newVisited newRequired) unvisitedNeighbors
                      return (sum results)
            modify' (HashMap.insert cacheKey pathCount)
            return pathCount
      result = evalState (countPaths "svr" Set.empty (Set.fromList ["dac", "fft"])) HashMap.empty
   in return result
