module Main where

import Data.List.Split (splitOn)
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "distance" $ do
    it "calculates the distance between two positions" $ do
      let pos1 = Position 0 0 0
          pos2 = Position 3 4 0
      distance pos1 pos2 `shouldBe` 5.0
      let pos3 = Position 1 2 2
          pos4 = Position 4 6 6
      distance pos3 pos4 `shouldBe` 6.4031242374328485

  describe "connectCircuit" $ do
    it "connects positions into circuits correctly" $ do
      let a = Position 0 0 0
          b = Position 1 1 1
          c = Position 2 2 2
          circuitsAfterAB = connectCircuit (Pair a b) []
          circuitsAfterAC = connectCircuit (Pair a c) circuitsAfterAB
          circuitsAfterBC = connectCircuit (Pair b c) circuitsAfterAC
      length circuitsAfterAB `shouldBe` 1
      length circuitsAfterAC `shouldBe` 1
      length circuitsAfterBC `shouldBe` 1
      let Circuit positions = head circuitsAfterBC
      positions `shouldContain` [a]
      positions `shouldContain` [b]
      positions `shouldContain` [c]

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ Position 162 817 812
            , Position 57 618 57
            , Position 906 360 560
            , Position 592 479 940
            , Position 352 342 300
            , Position 466 668 158
            , Position 542 29 236
            , Position 431 825 988
            , Position 739 650 466
            , Position 52 470 668
            , Position 216 146 977
            , Position 819 987 18
            , Position 117 168 530
            , Position 805 96 715
            , Position 346 949 466
            , Position 970 615 88
            , Position 941 993 340
            , Position 862 61 35
            , Position 984 92 344
            , Position 425 690 689
            ]
      puzzle1 10 input `shouldReturn` 40

    it "processes input file" $ do
      contents <- readFile "input"
      let parsePosition [x, y, z] = Position x y z
          parsePosition _ = error "Invalid position format"
          positions = map (parsePosition . map read . splitOn ",") (lines contents)
      result <- puzzle1 1000 positions
      pendingWith $ show result

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ Position 162 817 812
            , Position 57 618 57
            , Position 906 360 560
            , Position 592 479 940
            , Position 352 342 300
            , Position 466 668 158
            , Position 542 29 236
            , Position 431 825 988
            , Position 739 650 466
            , Position 52 470 668
            , Position 216 146 977
            , Position 819 987 18
            , Position 117 168 530
            , Position 805 96 715
            , Position 346 949 466
            , Position 970 615 88
            , Position 941 993 340
            , Position 862 61 35
            , Position 984 92 344
            , Position 425 690 689
            ]
      puzzle2 input `shouldReturn` 25272

    it "processes input file" $ do
      contents <- readFile "input"
      let parsePosition [x, y, z] = Position x y z
          parsePosition _ = error "Invalid position format"
          positions = map (parsePosition . map read . splitOn ",") (lines contents)
      result <- puzzle2 positions
      pendingWith $ show result