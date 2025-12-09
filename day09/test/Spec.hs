module Main where

import Data.List.Split (splitOn)
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ (7, 1)
            , (11, 1)
            , (11, 7)
            , (9, 7)
            , (9, 5)
            , (2, 5)
            , (2, 3)
            , (7, 3)
            ]
      puzzle1 input `shouldReturn` 50

    it "processes input file" $ do
      contents <- readFile "input"
      let parse [x, y] = (x, y)
          parse _ = error "Invalid position format"
          positions = map (parse . map read . splitOn ",") (lines contents)
      puzzle1 positions >>= \result -> pendingWith (show result)

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ (7, 1)
            , (11, 1)
            , (11, 7)
            , (9, 7)
            , (9, 5)
            , (2, 5)
            , (2, 3)
            , (7, 3)
            ]
      puzzle2 input `shouldReturn` 24

    it "processes input file" $ do
      contents <- readFile "input"
      let parse [x, y] = (x, y)
          parse _ = error "Invalid position format"
          positions = map (parse . map read . splitOn ",") (lines contents)
      puzzle2 positions >>= \result -> pendingWith (show result)