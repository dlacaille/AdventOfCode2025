module Spec where

import Solution
import Test.Hspec

spec :: IO ()
spec = hspec $ do
  describe "isInvalidId" $ do
    it "returns False for odd length strings" $ do
      isInvalidId "123" `shouldBe` False
      isInvalidId "000123" `shouldBe` False
    it "returns True for even length strings with identical halves" $ do
      isInvalidId "1212" `shouldBe` True
      isInvalidId "000000" `shouldBe` True
    it "returns False for even length strings with different halves" $ do
      isInvalidId "1234" `shouldBe` False
      isInvalidId "000123000" `shouldBe` False

  describe "isDivisorInvalidId" $ do
    it "returns True for two identical digits" $ do
      isDivisorInvalidId "11" `shouldBe` True
    it "returns True for three identical digits" $ do
      isDivisorInvalidId "111" `shouldBe` True
    it "returns True for pairs of identical digits" $ do
      isDivisorInvalidId "1212" `shouldBe` True

  describe "toRange" $ do
    it "parses a range string into a tuple" $ do
      toRange "100-200" `shouldBe` Range (100, 200)
      toRange "001-002" `shouldBe` Range (1, 2)

  describe "divisorsOf" $ do
    it "lists divisors of a given number" $ do
      divisorsOf 6 `shouldBe` [1, 2, 3, 6]
      divisorsOf 15 `shouldBe` [1, 3, 5, 15]

  describe "splitDivisors" $ do
    it "splits a list into sublists based on its divisors" $ do
      splitDivisors "abcd" `shouldBe` [["a", "b", "c", "d"], ["ab", "cd"]]
      splitDivisors "abcdef" `shouldBe` [["a", "b", "c", "d", "e", "f"], ["ab", "cd", "ef"], ["abc", "def"]]

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      puzzle1 "10-30,1000-1020" `shouldBe` 1043
      puzzle2 "1010-1020" `shouldBe` 1010
      puzzle1 "446443-446450" `shouldBe` 446446
    it "processes input file" $ do
      readFile "input" >>= pendingWith . show . puzzle1

  describe "puzzle2" $ do
    it "works for 2 digit ranges" $ do
      puzzle2 "11-22" `shouldBe` 11 + 22
    it "works for 3 digit ranges" $ do
      puzzle2 "95-115" `shouldBe` 99 + 111
    it "processes input file" $ do
      readFile "input" >>= pendingWith . show . puzzle2
