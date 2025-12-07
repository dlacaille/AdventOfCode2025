module Main where

import qualified Data.Map.Strict as Map
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "splitBeams" $ do
    it "creates starter beam" $ do
      splitBeams "..S.." (Map.fromList [(2, 1)]) `shouldBe` (Map.fromList [(2, 1)], 0)
    it "splits beams correctly" $ do
      splitBeams ".^.^." (Map.fromList [(1, 1)]) `shouldBe` (Map.fromList [(0, 1), (2, 1)], 1)
    it "merges beams" $ do
      splitBeams ".^.^." (Map.fromList [(1, 1), (3, 1)]) `shouldBe` (Map.fromList [(0, 1), (2, 2), (4, 1)], 2)
  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ ".......S......."
            , "..............."
            , ".......^......."
            , "..............."
            , "......^.^......"
            , "..............."
            , ".....^.^.^....."
            , "..............."
            , "....^.^...^...."
            , "..............."
            , "...^.^...^.^..."
            , "..............."
            , "..^...^.....^.."
            , "..............."
            , ".^.^.^.^.^...^."
            , "..............."
            ]
      puzzle1 input `shouldBe` 21
    it "processes input file" $ do
      contents <- readFile "input"
      let result = puzzle1 (lines contents)
      pendingWith $ show result
  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ ".......S......."
            , "..............."
            , ".......^......."
            , "..............."
            , "......^.^......"
            , "..............."
            , ".....^.^.^....."
            , "..............."
            , "....^.^...^...."
            , "..............."
            , "...^.^...^.^..."
            , "..............."
            , "..^...^.....^.."
            , "..............."
            , ".^.^.^.^.^...^."
            , "..............."
            ]
      puzzle2 input `shouldBe` 40
    it "processes input file" $ do
      contents <- readFile "input"
      let result = puzzle2 (lines contents)
      pendingWith (show result)