module Main where

import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "adjacentChars" $ do
    it "returns adjacent characters from a grid" $ do
      let grid =
            gridFromLines
              [ "12345"
              , "67890"
              , "abcde"
              , "fghij"
              , "klmno"
              ]
      adjacentChars 2 2 grid `shouldBe` "789bdghi"
      adjacentChars 0 0 grid `shouldBe` "267"
      adjacentChars 4 4 grid `shouldBe` "ijn"

  describe "countMatchingAdjacent" $ do
    it "counts adjacent characters matching the target character" $ do
      let grid =
            gridFromLines
              [ "..xx.xx@x."
              , "x@@.@.@.@@"
              , "@@@@@.x.@@"
              , "@.@@@@..@."
              , "x@.@@@@.@x"
              , ".@@@@@@@.@"
              , ".@.@.@.@@@"
              , "x.@@@.@@@@"
              , ".@@@@@@@@."
              , "x.x.@@@.x."
              ]
      countMatchingAdjacent 0 0 grid `shouldBe` 1
      countMatchingAdjacent 3 3 grid `shouldBe` 7

  describe "gridSize" $ do
    it "returns the size of the grid as (width, height)" $ do
      let grid =
            gridFromLines
              [ "abcd"
              , "efgh"
              , "ijkl"
              ]
      gridSize grid `shouldBe` (4, 3)

  describe "positionsOfChar" $ do
    it "finds all positions of a given character in the grid" $ do
      let grid =
            gridFromLines
              [ "abca"
              , "defa"
              , "ghia"
              ]
      positionsOfChar 'a' grid `shouldBe` [(0, 0), (3, 0), (3, 1), (3, 2)]
      positionsOfChar 'd' grid `shouldBe` [(0, 1)]

  describe "replaceCharAt" $ do
    it "replaces a character at a specific position in the grid" $ do
      let grid =
            gridFromLines
              [ "abcd"
              , "efgh"
              , "ijkl"
              ]
      charAt 1 1 (replaceCharAt 1 1 '.' grid) `shouldBe` Just '.'
      charAt 0 0 (replaceCharAt 0 0 '.' grid) `shouldBe` Just '.'

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ "..@@.@@@@."
            , "@@@.@.@.@@"
            , "@@@@@.@.@@"
            , "@.@@@@..@."
            , "@@.@@@@.@@"
            , ".@@@@@@@.@"
            , ".@.@.@.@@@"
            , "@.@@@.@@@@"
            , ".@@@@@@@@."
            , "@.@.@@@.@."
            ]
      puzzle1 '@' input `shouldBe` 13
    it "processes input file" $ do
      contents <- readFile "input"
      let result = puzzle1 '@' (lines contents)
      pendingWith $ show result

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ "..@@.@@@@."
            , "@@@.@.@.@@"
            , "@@@@@.@.@@"
            , "@.@@@@..@."
            , "@@.@@@@.@@"
            , ".@@@@@@@.@"
            , ".@.@.@.@@@"
            , "@.@@@.@@@@"
            , ".@@@@@@@@."
            , "@.@.@@@.@."
            ]
      puzzle2 '@' input `shouldBe` 43
    it "processes input file" $ do
      contents <- readFile "input"
      let result = puzzle2 '@' (lines contents)
      pendingWith $ show result
