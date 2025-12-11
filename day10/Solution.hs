{-# LANGUAGE BangPatterns #-}
module Solution where

import Control.Monad (msum)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace (trace)
import Data.Ord (Down(Down))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

type Button = [Int]
type State a = [a]
type NextButton a = Button -> [a] -> [a] -> [Button] -> [Button]

buttonsAfter :: NextButton Bool
buttonsAfter button _ _ = filter (> button)

checkState :: NextButton Int
checkState _ state desiredState =
  filter
    ( \button ->
        let newState = zipWith (\i s -> if i `elem` button then s + 1 else s) [0 ..] state
         in and $ zipWith (>=) desiredState newState
    )

solve :: (Eq a, Show a) => [Button] -> (a -> a) -> NextButton a -> [a] -> [a] -> [Button] -> [[Button]]
solve pressedButtons operation nextButtons initialState desiredState buttons
  | not (null solutions) = [button : pressedButtons | (_, button) <- take 1 solutions]
  | otherwise =
      msum
        [ let newPressed = (button : pressedButtons)
              newButtons = nextButtons button state desiredState buttons
           in solve newPressed operation nextButtons state desiredState newButtons
        | (state, button) <- nextStates
        ]
 where
  nextStates = [(zipWith (\i s -> if i `elem` btn then operation s else s) [0 ..] initialState, btn) | btn <- buttons]
  solutions = sortOn (length . snd) [(s, b) | (s, b) <- nextStates, s == desiredState]

start :: (Eq a, Show a) => ([a], [Button]) -> (a -> a) -> (Button -> [a] -> [a] -> [Button] -> [Button]) -> a -> Maybe [Button]
start (desiredState, buttons) operation nextButtons initialValue =
  listToMaybe $ sortOn length $ solve [] operation nextButtons initialState desiredState sortedButtons
 where
  initialState = replicate (length desiredState) initialValue
  sortedButtons = sortOn (Down . length) buttons

timedSolve :: (Eq a, Show a) => Int -> Int -> ([a], [Button]) -> (a -> a) -> (Button -> [a] -> [a] -> [Button] -> [Button]) -> a -> Maybe [Button]
timedSolve idx total machine operation nextButtons initialValue = unsafePerformIO $ do
  startTime <- getCurrentTime
  let result = start machine operation nextButtons initialValue
  let !forcedResult = case result of
                        Nothing -> Nothing
                        Just xs -> let !_ = length xs in Just xs
  endTime <- getCurrentTime
  let elapsed = diffUTCTime endTime startTime
  let msg = printf "Solved Machine %d/%d (%.2fs)" idx total (realToFrac elapsed :: Double)
  return $ trace msg forcedResult

puzzle1 :: [([Bool], [Button])] -> IO Int
puzzle1 machines = return . sum . map length . mapMaybe (\(idx, m) -> timedSolve idx total m not buttonsAfter False) $ zip [1 :: Int ..] machines
  where total = length machines

puzzle2 :: [([Int], [Button])] -> IO Int
puzzle2 machines = return . sum . map length $ results
  where
    total = length machines
    indexedMachines = zip [1 :: Int ..] machines
    results = mapMaybe (\(idx, m) -> timedSolve idx total m (+ 1) checkState 0) indexedMachines `using` parListChunk 8 rdeepseq
