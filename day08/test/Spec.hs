module Main where

import Data.List.Split (splitOn)
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "distance" $ do
    it "calculates the distance between two positions" $ do
      distance (0, 0, 0) (3, 4, 0) `shouldBe` 5.0
      distance (1, 2, 2) (4, 6, 6) `shouldBe` 6.4031242374328485

  describe "connect" $ do
    it "connects positions into circuits correctly" $ do
      let a = (0, 0, 0)
          b = (1, 1, 1)
          c = (2, 2, 2)
          circuits = connect (b, c) $ connect (a, c) $ connect (a, b) []
      length circuits `shouldBe` 1
      length (head circuits) `shouldBe` 3

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ (162, 817, 812)
            , (57, 618, 57)
            , (906, 360, 560)
            , (592, 479, 940)
            , (352, 342, 300)
            , (466, 668, 158)
            , (542, 29, 236)
            , (431, 825, 988)
            , (739, 650, 466)
            , (52, 470, 668)
            , (216, 146, 977)
            , (819, 987, 18)
            , (117, 168, 530)
            , (805, 96, 715)
            , (346, 949, 466)
            , (970, 615, 88)
            , (941, 993, 340)
            , (862, 61, 35)
            , (984, 92, 344)
            , (425, 690, 689)
            ]
      puzzle1 10 input `shouldBe` 40

    it "processes input file" $ do
      contents <- readFile "input"
      let parse [x, y, z] = (x, y, z)
          parse _ = error "Invalid position format"
          positions = map (parse . map read . splitOn ",") (lines contents)
      pendingWith $ show (puzzle1 1000 positions)

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ (162, 817, 812)
            , (57, 618, 57)
            , (906, 360, 560)
            , (592, 479, 940)
            , (352, 342, 300)
            , (466, 668, 158)
            , (542, 29, 236)
            , (431, 825, 988)
            , (739, 650, 466)
            , (52, 470, 668)
            , (216, 146, 977)
            , (819, 987, 18)
            , (117, 168, 530)
            , (805, 96, 715)
            , (346, 949, 466)
            , (970, 615, 88)
            , (941, 993, 340)
            , (862, 61, 35)
            , (984, 92, 344)
            , (425, 690, 689)
            ]
      puzzle2 input `shouldBe` 25272

    it "processes input file" $ do
      contents <- readFile "input"
      let parse [x, y, z] = (x, y, z)
          parse _ = error "Invalid position format"
          positions = map (parse . map read . splitOn ",") (lines contents)
      pendingWith $ show (puzzle2 positions)