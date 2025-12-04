module Spec where

import Solution
import Test.Hspec

spec :: IO ()
spec = hspec $ do
  describe "adjacentChars" $ do
    it "returns adjacent characters from a grid" $ do
      let input =
            [ "12345"
            , "67890"
            , "abcde"
            , "fghij"
            , "klmno"
            ]
      adjacentChars input (2, 2) `shouldBe` "789bdghi"
      adjacentChars input (0, 0) `shouldBe` "267"
      adjacentChars input (4, 4) `shouldBe` "ijn"

  describe "countMatchingAdjacent" $ do
    it "counts adjacent characters matching the target character" $ do
      let input =
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
      countMatchingAdjacent input (0, 0) `shouldBe` 1
      countMatchingAdjacent input (3, 3) `shouldBe` 7

  describe "positionsOfChar" $ do
    it "finds all positions of a given character in the grid" $ do
      let input =
            [ "abca"
            , "defa"
            , "ghia"
            ]
      positionsOfChar input 'a' `shouldBe` [(0, 0), (3, 0), (3, 1), (3, 2)]
      positionsOfChar input 'd' `shouldBe` [(0, 1)]

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
      puzzle1 input '@' `shouldBe` 13
    it "processes input file" $ readFile "input" >>= pendingWith . show . (`puzzle1` '@') . lines

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
      puzzle2 input '@' `shouldBe` 43
    it "processes input file" $ readFile "input" >>= pendingWith . show . (`puzzle2` '@') . lines

-- it "processes input file" $ readFile "input" >>= pendingWith . show . puzzle2 . lines
