module Main where

import System.IO

rotateDial :: Int -> Char -> Int -> Int
rotateDial dial 'L' x = (dial - x) `mod` 100
rotateDial dial 'R' x = (dial + x) `mod` 100
rotateDial dial _ _ = dial

countZero :: Int -> Int -> Int
countZero acc 0 = acc + 1
countZero acc _ = acc

countTimesPassedZero :: Int -> Char -> Int -> Int
countTimesPassedZero dial 'L' x = (x - dial) `div` 100 + (if dial > 0 then 1 else 0)
countTimesPassedZero dial 'R' x = (x + dial) `div` 100
countTimesPassedZero _ _ _ = 0

parseLine :: String -> (Char, Int)
parseLine [] = ('R', 0)
parseLine l = let (x : xs) = l in (x, read xs)

parseAndRotate :: Int -> String -> Int
parseAndRotate dial l = let (dir, x) = parseLine l in rotateDial dial dir x

parseAndRotateAndCountZero :: (Int, Int) -> String -> (Int, Int)
parseAndRotateAndCountZero (dial, acc) l =
    let newDial = parseAndRotate dial l
        newAcc = countZero acc newDial
     in (newDial, newAcc)

parseAndRotateAndCountPassedZero :: (Int, Int) -> String -> (Int, Int)
parseAndRotateAndCountPassedZero (dial, acc) l =
    let (dir, x) = parseLine l
        newDial = rotateDial dial dir x
        newAcc = acc + countTimesPassedZero dial dir x
     in (newDial, newAcc)

puzzle1 :: [String] -> Int
puzzle1 l = snd $ foldl parseAndRotateAndCountZero (50, 0) l

puzzle2 :: [String] -> Int
puzzle2 l = snd $ foldl parseAndRotateAndCountPassedZero (50, 0) l

main :: IO ()
main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    putStr $ "Puzzle 1: " ++ show (puzzle1 $ lines contents)
    putStr $ "Puzzle 2: " ++ show (puzzle2 $ lines contents)
