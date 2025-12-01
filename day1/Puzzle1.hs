module Puzzle1 where

import System.IO
import Lib

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStr $ "Puzzle 1: " ++ show (puzzle1 $ lines contents)
