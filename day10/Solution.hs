module Solution where

import Control.Monad (msum)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)

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
  listToMaybe $ sortOn length $ solve [] operation nextButtons initialState desiredState (reverse buttons)
 where
  initialState = replicate (length desiredState) initialValue

puzzle1 :: [([Bool], [Button])] -> IO Int
puzzle1 = return . sum . map length . mapMaybe (\m -> start m not buttonsAfter False)

puzzle2 :: [([Int], [Button])] -> IO Int
puzzle2 = return . sum . map length . mapMaybe (\m -> start m (+ 1) checkState 0)
