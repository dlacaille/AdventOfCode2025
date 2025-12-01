module Solution where

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

puzzle1 :: [String] -> Int
puzzle1 = snd . foldl rotate (50, 0) . map parseLine
 where
  rotate (dial, acc) (dir, x) =
    let newDial = rotateDial dial dir x
        countZeroes = acc + if newDial == 0 then 1 else 0
     in (newDial, countZeroes)

puzzle2 :: [String] -> Int
puzzle2 = snd . foldl rotate (50, 0) . map parseLine
 where
  rotate (dial, acc) (dir, x) =
    let newDial = rotateDial dial dir x
        countZeroes = acc + countTimesPassedZero dial dir x
     in (newDial, countZeroes)
