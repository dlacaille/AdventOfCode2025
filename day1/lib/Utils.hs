module Utils where

parseLine :: String -> (Char, Int)
parseLine [] = ('R', 0)
parseLine l = let (x : xs) = l in (x, read xs)

rotateDial :: Int -> Char -> Int -> Int
rotateDial dial 'L' x = (dial - x) `mod` 100
rotateDial dial 'R' x = (dial + x) `mod` 100
rotateDial dial _ _ = dial

countTimesPassedZero :: Int -> Char -> Int -> Int
countTimesPassedZero dial 'L' x = (x - dial) `div` 100 + (if dial > 0 then 1 else 0)
countTimesPassedZero dial 'R' x = (x + dial) `div` 100
countTimesPassedZero _ _ _ = 0
