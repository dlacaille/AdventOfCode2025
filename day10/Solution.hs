{-# LANGUAGE BangPatterns #-}

module Solution where

import Control.Monad (msum)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down (Down))

type Button = [Int]
type State a = [a]
type NextButton a = Button -> [a] -> [a] -> [Button] -> [Button]

buttonsAfter :: NextButton Bool
buttonsAfter button _ _ = filter (> button)

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
  currentState = foldl (\s btn -> zipWith (\i val -> if i `elem` btn then not val else val) [0 ..] s) (replicate (length desiredState) False) pressedButtons
  nextStates = [(zipWith (\i s -> if i `elem` btn then not s else s) [0 ..] currentState, btn) | btn <- buttons]
  solutions = sortOn (length . snd) [(s, b) | (s, b) <- nextStates, s == desiredState]

patterns :: [[Int]] -> Map [Int] Int
patterns coeffs = Map.fromListWith min allPatterns
 where
  numButtons = length coeffs
  numVariables = if null coeffs then 0 else length (head coeffs)
  zeroVector = replicate numVariables 0
  allPatterns =
    [ (pattern, patternLen)
    | patternLen <- [0 .. numButtons]
    , buttons <- subsequencesOfSize patternLen [0 .. numButtons - 1]
    , let pattern = foldl (zipWith (+)) zeroVector [coeffs !! i | i <- buttons]
    ]
  subsequencesOfSize 0 _ = [[]]
  subsequencesOfSize _ [] = []
  subsequencesOfSize n (x : xs) = map (x :) (subsequencesOfSize (n - 1) xs) ++ subsequencesOfSize n xs

solve2 :: [Button] -> [Int] -> [Button] -> Maybe [Button]
solve2 _ desiredState buttons =
  let !coeffs = map toVector buttons
      !n = length desiredState
      !patternCosts = patterns coeffs
      toVector btn = [if i `elem` btn then 1 else 0 | i <- [0 .. n - 1]]

      solveMemo :: Map [Int] Int -> [Int] -> (Int, Map [Int] Int)
      solveMemo memo goal
        | all (== 0) goal = (0, memo)
        | Just cached <- Map.lookup goal memo = (cached, memo)
        | otherwise =
            let (answer, finalMemo) = foldl tryPattern (1000000, memo) (Map.toList patternCosts)
             in (answer, Map.insert goal answer finalMemo)
       where
        tryPattern (bestSoFar, currentMemo) (pattern, patternCost)
          | all (\(p, g) -> p <= g && p `mod` 2 == g `mod` 2) (zip pattern goal) =
              let newGoal = zipWith (\g p -> (g - p) `div` 2) goal pattern
                  (subResult, newMemo) = solveMemo currentMemo newGoal
                  candidate = patternCost + 2 * subResult
               in (min bestSoFar candidate, newMemo)
          | otherwise = (bestSoFar, currentMemo)

      (result, _) = solveMemo Map.empty desiredState
   in if result >= 1000000 then Nothing else Just (replicate result [0])

puzzle1 :: [([Bool], [Button])] -> IO Int
puzzle1 machines = return . sum . map length $ mapMaybe start machines
 where
  start (desiredState, buttons) =
    let sortedButtons = sortOn (Down . length) buttons
     in listToMaybe $ sortOn length $ solve1 [] desiredState sortedButtons

puzzle2 :: [([Int], [Button])] -> IO Int
puzzle2 machines = return . sum . map length $ mapMaybe start machines
 where
  start (desiredState, buttons) =
    let sortedButtons = sortOn (Down . length) buttons
     in solve2 [] desiredState sortedButtons