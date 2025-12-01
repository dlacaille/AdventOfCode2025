module Main where

import Lib
import System.IO

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStr $ "Puzzle 1: " ++ show (puzzle1 $ lines contents)
