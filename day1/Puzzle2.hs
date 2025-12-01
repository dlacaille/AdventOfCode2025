module Main where

import System.IO
import Utils

puzzle2 :: [String] -> Int
puzzle2 = snd . foldl rotate (50, 0) . map parseLine
 where
  rotate (dial, acc) (dir, x) =
    let newDial = rotateDial dial dir x
        countZeroes = acc + countTimesPassedZero dial dir x
     in (newDial, countZeroes)

main :: IO ()
main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStr $ "Puzzle 2: " ++ show (puzzle2 $ lines contents)
