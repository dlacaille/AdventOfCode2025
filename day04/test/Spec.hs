module Spec where

import Solution
import Test.Hspec

spec :: IO ()
spec = hspec $ do
  describe "adjacentChars" $ do
    it "returns adjacent characters from a grid" $ do
      let input =
            gridFromLines
              [ "12345"
              , "67890"
              , "abcde"
              , "fghij"
              , "klmno"
              ]
      adjacentChars 2 2 input `shouldBe` "789bdghi"
      adjacentChars 0 0 input `shouldBe` "267"
      adjacentChars 4 4 input `shouldBe` "ijn"

  describe "countMatchingAdjacent" $ do
    it "counts adjacent characters matching the target character" $ do
      let input =
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
      countMatchingAdjacent 0 0 input `shouldBe` 1
      countMatchingAdjacent 3 3 input `shouldBe` 7

  describe "gridSize" $ do
    it "returns the size of the grid as (width, height)" $ do
      let input =
            gridFromLines
              [ "abcd"
              , "efgh"
              , "ijkl"
              ]
      gridSize input `shouldBe` (4, 3)

  describe "positionsOfChar" $ do
    it "finds all positions of a given character in the grid" $ do
      let input =
            gridFromLines
              [ "abca"
              , "defa"
              , "ghia"
              ]
      positionsOfChar 'a' input `shouldBe` [(0, 0), (3, 0), (3, 1), (3, 2)]
      positionsOfChar 'd' input `shouldBe` [(0, 1)]

  describe "replaceCharAt" $ do
    it "replaces a character at a specific position in the grid" $ do
      let input =
            gridFromLines
              [ "abcd"
              , "efgh"
              , "ijkl"
              ]
      replaceCharAt 1 1 '.' input `shouldBe` gridFromLines ["abcd", "e.gh", "ijkl"]
      replaceCharAt 0 0 '.' input `shouldBe` gridFromLines [".bcd", "efgh", "ijkl"]

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let input =
            gridFromLines
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
      puzzle1 '@' input `shouldReturn` 13
    it "processes input file" $ do
      contents <- readFile "input"
      result <- puzzle1 '@' (gridFromLines $ lines contents)
      pendingWith $ show result

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            gridFromLines
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
      puzzle2 '@' input `shouldReturn` 43
    it "processes input file" $ do
      contents <- readFile "input"
      result <- puzzle2 '@' (gridFromLines $ lines contents)
      pendingWith $ show result
