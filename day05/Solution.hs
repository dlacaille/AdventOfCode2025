module Solution where

import Data.List (sortBy)

type Range = (Int, Int)

getRange :: String -> Maybe Range
getRange str =
  case span (/= '-') str of
    (a, '-' : b) -> Just (read a, read b)
    _ -> Nothing

rangeSize :: Range -> Int
rangeSize (start, end) = end - start + 1

isOverlapping :: Range -> Range -> Bool
isOverlapping (start1, end1) (start2, end2) = not (end1 < start2 || end2 < start1)

combineRanges :: Range -> Range -> Range
combineRanges (start1, end1) (start2, end2) = (min start1 start2, max end1 end2)

orderRanges :: [Range] -> [Range]
orderRanges = sortBy (\(a, _) (b, _) -> compare a b)

reduceRanges :: [Range] -> [Range]
reduceRanges [] = []
reduceRanges (r : rs) = reverse $ foldl merge [r] rs
 where
  merge [] y = [y]
  merge acc@(x : xs) y
    | isOverlapping x y = combineRanges x y : xs
    | otherwise = y : acc

isInRange :: Int -> Range -> Bool
isInRange n (start, end) = n >= start && n <= end

puzzle1 :: [Range] -> [Int] -> Int
puzzle1 ranges ingredients = length [n | n <- ingredients, any (isInRange n) ranges]

puzzle2 :: [Range] -> Int
puzzle2 ranges = sum $ map rangeSize $ reduceRanges $ orderRanges ranges