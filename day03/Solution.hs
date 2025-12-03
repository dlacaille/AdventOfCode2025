module Solution where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

findBiggestNumberWithIndex :: [Char] -> (Char, Int)
findBiggestNumberWithIndex xs =
  let m = maximum xs
   in (m, fromJust $ elemIndex m xs)

findLargestNumber :: Int -> [Char] -> Int
findLargestNumber digits xs =
  let (x, idx) = findBiggestNumberWithIndex $ take (length xs - digits + 1) xs
   in if digits > 1
        then read $ x : show (findLargestNumber (digits - 1) (drop (idx + 1) xs))
        else read [x]

puzzle1 :: [[Char]] -> Int
puzzle1 xs = sum $ map (findLargestNumber 2) xs

puzzle2 :: [[Char]] -> Int
puzzle2 xs = sum $ map (findLargestNumber 12) xs
