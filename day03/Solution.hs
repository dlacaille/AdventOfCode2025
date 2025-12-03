module Solution where

import Data.List (maximumBy)
import Data.Ord (comparing)

findLargestDigitWithIndex :: [Char] -> (Char, Int)
findLargestDigitWithIndex xs = maximumBy (comparing fst) (reverse $ zip xs [0 ..])

findLargestNumberInString :: Int -> [Char] -> String
findLargestNumberInString 0 _ = ""
findLargestNumberInString digits xs =
  let (x, idx) = findLargestDigitWithIndex $ take (length xs - digits + 1) xs
   in x : findLargestNumberInString (digits - 1) (drop (idx + 1) xs)

puzzle1 :: [[Char]] -> Int
puzzle1 = sum . map (read . findLargestNumberInString 2)

puzzle2 :: [[Char]] -> Int
puzzle2 = sum . map (read . findLargestNumberInString 12)
