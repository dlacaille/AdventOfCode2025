module Solution where

import Data.Ix.Enum (range)
import Data.List.Split (chunksOf, splitOn)

divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n], n `mod` x == 0]

isRepeating :: (Eq a) => Int -> [a] -> Bool
isRepeating 0 _ = False
isRepeating n a =
  case chunksOf n a of
    (x : xs) -> all (== x) xs
    _ -> False

isRepeatedTwice :: (Eq a) => [a] -> Bool
isRepeatedTwice s
  | odd (length s) = False
  | otherwise = isRepeating (length s `div` 2) s

isPeriodic :: (Eq a) => [a] -> Bool
isPeriodic s = any (`isRepeating` s) $ (init . divisors) (length s)

toRange :: String -> (Int, Int)
toRange s =
  case splitOn "-" s of
    [start, end] -> (read start, read end)
    _ -> (0, 0)

puzzle1 :: [(Int, Int)] -> Int
puzzle1 =
  sum . map read . concatMap (filter isRepeatedTwice . map show . range)

puzzle2 :: [(Int, Int)] -> Int
puzzle2 =
  sum . map read . concatMap (filter isPeriodic . map show . range)
