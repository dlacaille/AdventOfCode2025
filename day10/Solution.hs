{-# LANGUAGE BangPatterns #-}
module Solution where

import Control.Monad (msum)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.List (sortOn, minimumBy, subsequences)
import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace (trace)
import Data.Ord (Down(Down), comparing)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

type Button = [Int]
type State a = [a]
type NextButton a = Button -> [a] -> [a] -> [Button] -> [Button]

buttonsAfter :: NextButton Bool
buttonsAfter button _ _ = filter (> button)

-- Solve for puzzle1 (Bool) using backtracking search
solve1 :: [Button] -> [Bool] -> [Button] -> [[Button]]
solve1 pressedButtons desiredState buttons
  | not (null solutions) = [button : pressedButtons | (_, button) <- take 1 solutions]
  | otherwise =
      msum
        [ let newPressed = (button : pressedButtons)
              newButtons = filter (> button) buttons
           in solve1 newPressed desiredState newButtons
        | (_, button) <- nextStates
        ]
 where
  currentState = foldl (\s btn -> zipWith (\i val -> if i `elem` btn then not val else val) [0..] s) (replicate (length desiredState) False) pressedButtons
  nextStates = [(zipWith (\i s -> if i `elem` btn then not s else s) [0 ..] currentState, btn) | btn <- buttons]
  solutions = sortOn (length . snd) [(s, b) | (s, b) <- nextStates, s == desiredState]

solve2 :: [Button] -> [Int] -> [Button] -> Maybe [Button]
solve2 _ desiredState buttons
  | all (== 0) desiredState = Just []
  | otherwise = minimumByMaybe (comparing length) . mapMaybe trySolve $ subsequences (zip buttons buttonVectors)
  where
    !n = length desiredState
    !buttonVectors = map toVector buttons
    toVector btn = [if i `elem` btn then 1 else 0 | i <- [0 .. n - 1]]

    minimumByMaybe _ [] = Nothing
    minimumByMaybe cmp xs = Just (minimumBy cmp xs)

    trySolve [] = if all (== 0) desiredState then Just [] else Nothing
    trySolve combo = listToMaybe $ tryAllCounts (unzip combo) desiredState

    tryAllCounts ([], []) target
      | all (== 0) target = [[]]
      | otherwise = []
    tryAllCounts (btn : btns, vec : vecs) target =
      [ replicate count btn ++ rest
      | count <- [0 .. maxCount vec target]
      , let !newTarget = zipWith (-) target (map (* count) vec)
      , all (>= 0) newTarget
      , rest <- tryAllCounts (btns, vecs) newTarget
      ]
    tryAllCounts _ _ = []

    maxCount vec target = 
      let !result = minimum [if v > 0 then target !! i `div` v else maxBound | (i, v) <- zip [0 ..] vec, v > 0]
      in result

start1 :: ([Bool], [Button]) -> Maybe [Button]
start1 (desiredState, buttons) =
  listToMaybe $ sortOn length $ solve1 [] desiredState sortedButtons
 where
  sortedButtons = sortOn (Down . length) buttons

start2 :: ([Int], [Button]) -> Maybe [Button]
start2 (desiredState, buttons) =
  solve2 [] desiredState sortedButtons
 where
  sortedButtons = sortOn (Down . length) buttons

timedSolve :: Int -> Int -> (a -> b) -> a -> b
timedSolve idx total operation input = unsafePerformIO $ do
  startTime <- getCurrentTime
  let !result = operation input
  endTime <- getCurrentTime
  let elapsed = diffUTCTime endTime startTime
  let msg = printf "Solved Machine %d/%d (%.2fs)" idx total (realToFrac elapsed :: Double)
  return $ trace msg result

puzzle1 :: [([Bool], [Button])] -> IO Int
puzzle1 machines = return . sum . map length $ results
  where total = length machines
        indexedMachines = zip [1 :: Int ..] machines
        results = mapMaybe (\(idx, m) -> timedSolve idx total start1 m) indexedMachines

puzzle2 :: [([Int], [Button])] -> IO Int
puzzle2 machines = return . sum . map length $ results
  where
    total = length machines
    sortedMachines = sortOn (length . fst) machines
    indexedMachines = zip [1 :: Int ..] sortedMachines
    results = mapMaybe (\(idx, m) -> timedSolve idx total start2 m) indexedMachines `using` parListChunk 4 rdeepseq
