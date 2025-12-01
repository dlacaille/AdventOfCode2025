module Main where

import System.IO
import Utils

puzzle1 :: [String] -> Int
puzzle1 = snd . foldl rotate (50, 0) . map parseLine
 where
  rotate (dial, acc) (dir, x) =
    let newDial = rotateDial dial dir x
        countZeroes = acc + if newDial == 0 then 1 else 0
     in (newDial, countZeroes)

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStr $ "Puzzle 1: " ++ show (puzzle1 $ lines contents)
