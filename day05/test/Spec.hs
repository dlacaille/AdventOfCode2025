module Main where

import Data.Maybe (fromJust)
import Solution
import Test.Hspec
import Text.Read (readMaybe)

main :: IO ()
main = hspec $ do
  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let ranges = [(3, 5), (10, 14), (16, 20), (12, 18)]
      let ingredients = [1, 5, 8, 11, 17, 32]
      puzzle1 ranges ingredients `shouldBe` 3
    it "processes input file" $ do
      contents <- readFile "input"
      let (rangeLines, ingredientLines) = break null (lines contents)
      let ranges = map (fromJust . getRange) rangeLines
      let ingredients = map (fromJust . readMaybe) (tail ingredientLines)
      let result = puzzle1 ranges ingredients
      pendingWith $ show result

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let ranges = [(3, 5), (10, 14), (16, 20), (12, 18)]
      puzzle2 ranges `shouldBe` 14
    it "processes input file" $ do
      contents <- readFile "input"
      let (rangeLines, _) = break null (lines contents)
      let ranges = map (fromJust . getRange) rangeLines
      let result = puzzle2 ranges
      pendingWith $ show result
