module Main where

import Data.Char (toUpper)
import System.IO

rotateDialLeft :: Int -> Int -> Int
rotateDialLeft dial x = (dial - x) `mod` 100

rotateDialRight :: Int -> Int -> Int
rotateDialRight dial x = (dial + x) `mod` 100

rotateDial :: Int -> Char -> Int -> Int
rotateDial dial 'L' x = rotateDialLeft dial x
rotateDial dial 'R' x = rotateDialRight dial x

countZero :: Int -> Int -> Int
countZero acc 0 = acc + 1
countZero acc _ = acc

parseLine :: String -> (Char, Int)
parseLine l = let (x:xs) = l in (toUpper x, read xs)

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
        timesPassedZero = case dir of
            'L' -> (x - dial) `div` 100 + (if dial > 0 then 1 else 0)
            'R' -> (x + dial) `div` 100
        newDial = rotateDial dial dir x
        newAcc = acc + timesPassedZero
    in (newDial, newAcc)

puzzle1 :: IO ()
puzzle1 = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    print $ snd $ foldl parseAndRotateAndCountZero (50, 0) (lines contents)
    
puzzle2 :: IO ()
puzzle2 = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    print $ snd $ foldl parseAndRotateAndCountPassedZero (50, 0) (lines contents)
    
main :: IO ()
main = do
    putStr "Puzzle 1: "
    puzzle1
    putStr "Puzzle 2: "
    puzzle2
