module Solution where

import Control.Monad (msum)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)

type Indicator = Bool
type Button = [Int]
data Machine = Machine [Indicator] [Button] deriving (Show, Eq)

toggleButton :: Button -> [Indicator] -> [Indicator]
toggleButton button = zipWith (\i s -> if i `elem` button then not s else s) [0 ..]

solve :: [Button] -> [Indicator] -> [Indicator] -> [Button] -> [[Button]]
solve pressedButtons initialState desiredState buttons
  | not (null solutions) = [button : pressedButtons | (_, button) <- take 1 solutions]
  | otherwise = msum [solve (button : pressedButtons) state desiredState (filter (> button) buttons) | (state, button) <- nextStates]
 where
  nextStates = [(toggleButton btn initialState, btn) | btn <- buttons]
  solutions = sortOn (length . snd) [(s, b) | (s, b) <- nextStates, s == desiredState]

start :: Machine -> Maybe [Button]
start (Machine indicators buttons) =
  listToMaybe $ sortOn length $ solve [] initialState indicators (reverse buttons)
 where
  initialState = replicate (length indicators) False

puzzle1 :: [Machine] -> IO Int
puzzle1 = return . sum . map length . mapMaybe start

puzzle2 :: IO Int
puzzle2 = return 0