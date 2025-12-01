module Solution where

data Dir = L | R deriving (Show, Eq)

toDir :: Char -> Dir
toDir 'L' = L
toDir 'R' = R
toDir _ = R

parseLine :: String -> (Dir, Int)
parseLine [] = (R, 0)
parseLine l = let (x : xs) = l in (toDir x, read xs)

rotateDial :: Int -> Dir -> Int -> Int
rotateDial dial L x = (dial - x) `mod` 100
rotateDial dial R x = (dial + x) `mod` 100

countTimesPassedZero :: Int -> Dir -> Int -> Int
countTimesPassedZero dial L x = (x - dial) `div` 100 + (if dial > 0 then 1 else 0)
countTimesPassedZero dial R x = (x + dial) `div` 100

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
