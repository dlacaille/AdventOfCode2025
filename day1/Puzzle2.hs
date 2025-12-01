module Main where

import Lib
import System.IO

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStr $ "Puzzle 2: " ++ show (puzzle2 $ lines contents)
