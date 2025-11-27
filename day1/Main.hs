module Main where

import Data.Char (toUpper)

-- Adds an exclamation mark to the end of a string
exclaim :: String -> String
exclaim a = a ++ "!"

-- Capitalizes the first letter of a string
capitalize :: String -> String
capitalize (x : xs) = toUpper x : xs
capitalize [] = []

-- Capitalizes the first letter of each word in a sentence
capitalizeWords :: String -> String
capitalizeWords a = unwords $ map capitalize $ words a

main :: IO ()
main = putStrLn $ exclaim $ capitalizeWords "hello world"
