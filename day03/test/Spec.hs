module Spec where

import Solution
import Test.Hspec

spec :: IO ()
spec = hspec $ do
  describe "findLargestDigitWithIndex" $ it "finds the biggest number and its index in a list" $ do
    findLargestDigitWithIndex "12345" `shouldBe` ('5', 4)
    findLargestDigitWithIndex "54321" `shouldBe` ('5', 0)
    findLargestDigitWithIndex "13254" `shouldBe` ('5', 3)
    findLargestDigitWithIndex "7777" `shouldBe` ('7', 0)

  describe "findLargestNumberInString" $ do
    it "finds the largest number with given digits" $ do
      findLargestNumberInString 2 "987654321" `shouldBe` "98"
      findLargestNumberInString 3 "965487321" `shouldBe` "987"
      findLargestNumberInString 4 "123456789" `shouldBe` "6789"
      findLargestNumberInString 5 "546321789" `shouldBe` "63789"

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      puzzle1 ["987654321111111"] `shouldBe` 98
      puzzle1 ["811111111111119"] `shouldBe` 89
      puzzle1 ["234234234234278"] `shouldBe` 78
      puzzle1 ["818181911112111"] `shouldBe` 92
    it "processes input file" $ readFile "input" >>= pendingWith . show . puzzle1 . lines

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      puzzle2 ["987654321111111"] `shouldBe` 987654321111
      puzzle2 ["811111111111119"] `shouldBe` 811111111119
      puzzle2 ["234234234234278"] `shouldBe` 434234234278
      puzzle2 ["818181911112111"] `shouldBe` 888911112111
    it "processes input file" $ readFile "input" >>= pendingWith . show . puzzle2 . lines
