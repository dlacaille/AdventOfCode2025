module Main where

import Solution (puzzle2)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  print "Solving Puzzle 2..."
  contents <- readFile "input"
  let parseMachine :: String -> ([Int], [[Int]])
      parseMachine str =
        case words str of
          [] -> ([], [])
          (_ : buttonsStr) ->
            let joltages = map read (splitOn [','] $ tail $ init $ last buttonsStr)
                buttons = map (map read . splitOn [','] . tail . init) (init buttonsStr)
              in (joltages, buttons)
      machines = map parseMachine $ lines contents
  result <- puzzle2 machines
  print ("Puzzle 2 Answer is: " ++ show result)
