module Solution where

import Data.List.Split (chunksOf, splitOn)

trimLeadingZeroes :: String -> String
trimLeadingZeroes [] = []
trimLeadingZeroes s = dropWhile (== '0') s

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

divisorsOf :: Int -> [Int]
divisorsOf n = [x | x <- [1 .. n], n `mod` x == 0]

splitDivisors :: [a] -> [[[a]]]
splitDivisors xs = map (`chunksOf` xs) (init $ divisorsOf (length xs))

isInvalidId :: String -> Bool
isInvalidId [] = True
isInvalidId s =
  let trimmed = trimLeadingZeroes s
   in if (length trimmed) `mod` 2 == 1
        then False
        else
          let (firstHalf, secondHalf) = splitHalf trimmed
           in firstHalf == secondHalf

isDivisorInvalidId :: String -> Bool
isDivisorInvalidId [] = True
isDivisorInvalidId s =
  let trimmed = trimLeadingZeroes s
      split = splitDivisors trimmed
   in any invalid split
 where
  invalid a = case a of
    (x : xs) -> all (== x) xs
    _ -> False

parseRange :: String -> (Int, Int)
parseRange s =
  case splitOn "-" s of
    [start, end] -> (read start, read end)
    _ -> (0, 0)

findInvalidIdsInRange :: (Int, Int) -> [String]
findInvalidIdsInRange (start, end) =
  [ show n
  | n <- [start .. end]
  , isInvalidId (show n)
  ]

findDivisorInvalidIdsInRange :: (Int, Int) -> [String]
findDivisorInvalidIdsInRange (start, end) =
  [ show n
  | n <- [start .. end]
  , isDivisorInvalidId (show n)
  ]

puzzle1 :: String -> Int
puzzle1 input = do
  let ranges = map parseRange $ splitOn "," input
  sum $ map read $ concatMap findInvalidIdsInRange ranges

puzzle2 :: String -> Int
puzzle2 input = do
  let ranges = map parseRange $ splitOn "," input
  sum $ map read $ concatMap findDivisorInvalidIdsInRange ranges
