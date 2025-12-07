module Solution where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

splitBeams :: [Char] -> Map Int Int -> (Map Int Int, Int)
splitBeams line beamCounts =
  foldl
    ( \(acc, splitCount) (i, c) -> case c of
        'S' -> (Map.insertWith (+) i 1 acc, splitCount)
        '^' ->
          case Map.lookup i beamCounts of
            Just count ->
              let acc' = Map.insertWith (+) (i - 1) count acc
                  acc'' = Map.insertWith (+) (i + 1) count acc'
               in (acc'', splitCount + 1)
            Nothing -> (acc, splitCount)
        _ ->
          case Map.lookup i beamCounts of
            Just count -> (Map.insertWith (+) i count acc, splitCount)
            Nothing -> (acc, splitCount)
    )
    (Map.empty, 0)
    (zip [0 ..] line)

puzzle1 :: [[Char]] -> Int
puzzle1 l =
  snd $
    foldl
      ( \(beamCounts, splitCount) line ->
          let (newBeamCounts, newSplitCount) = splitBeams line beamCounts
           in (newBeamCounts, splitCount + newSplitCount)
      )
      (Map.empty, 0)
      l

puzzle2 :: [[Char]] -> Int
puzzle2 l =
  let (finalBeamCounts, _) =
        foldl
          ( \(beamCounts, splitCount) line ->
              let (newBeamCounts, newSplitCount) = splitBeams line beamCounts
               in (newBeamCounts, splitCount + newSplitCount)
          )
          (Map.empty, 0)
          l
   in sum (Map.elems finalBeamCounts)