{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.List.Split (splitOn)
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "puzzle1" $ do
    it "processes input file" $ do
      content <- readFile "input"
      let parsed = splitOn [""] $ lines content
          parsePresentShape :: [String] -> PresentShape
          parsePresentShape = map (map (== '#')) . tail
          presentShapes = map parsePresentShape $ init parsed
          parseRegion :: String -> Region
          parseRegion line =
            let (sizeStr : rest) = words line
                [widthStr, heightStr] = splitOn "x" $ init sizeStr
                width = read widthStr
                height = read heightStr
                counts = map read $ rest
             in (width, height, counts)
          regions = map parseRegion $ last parsed
      result <- puzzle1 presentShapes regions
      pendingWith $ show result

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      pending

    it "processes input file" $ do
      pending