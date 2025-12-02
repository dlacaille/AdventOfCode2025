module Solution where

import Data.List.Split (chunksOf, splitOn)

newtype Range = Range (Int, Int) deriving (Show, Eq)

divisorsOf :: Int -> [Int]
divisorsOf n = [x | x <- [1 .. n], n `mod` x == 0]

splitDivisors :: [a] -> [[[a]]]
splitDivisors xs = map (`chunksOf` xs) (init $ divisorsOf (length xs))

isInvalidId :: String -> Bool
isInvalidId [] = True
isInvalidId s = do
  let trimmed = dropWhile (== '0') s
  even (length trimmed)
    && let (firstHalf, secondHalf) = splitAt (length trimmed `div` 2) trimmed
        in firstHalf == secondHalf

isDivisorInvalidId :: String -> Bool
isDivisorInvalidId [] = True
isDivisorInvalidId s =
  let trimmed = dropWhile (== '0') s
      split = splitDivisors trimmed
   in any invalid split
 where
  invalid a = case a of
    (x : xs) -> all (== x) xs
    _ -> False

toRange :: String -> Range
toRange s =
  case splitOn "-" s of
    [start, end] -> Range (read start, read end)
    _ -> Range (0, 0)

puzzle1 :: String -> Int
puzzle1 input = do
  let ranges = map toRange $ splitOn "," input
  sum $ map read $ concatMap invalidIdsInRange ranges
 where
  invalidIdsInRange (Range (start, end)) =
    [show n | n <- [start .. end], isInvalidId (show n)]

puzzle2 :: String -> Int
puzzle2 input = do
  let ranges = map toRange $ splitOn "," input
  sum $ map read $ concatMap invalidIdsInRange ranges
 where
  invalidIdsInRange (Range (start, end)) =
    [show n | n <- [start .. end], isDivisorInvalidId (show n)]
