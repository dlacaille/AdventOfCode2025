module Spec where

import Solution
import Test.Hspec

spec :: IO ()
spec = hspec $ do
  describe "isInvalidId" $ do
    it "returns False for odd length strings" $ do
      isRepeatedTwice "123" `shouldBe` False
      isRepeatedTwice "000123" `shouldBe` False
    it "returns True for even length strings with identical halves" $ do
      isRepeatedTwice "1212" `shouldBe` True
      isRepeatedTwice "000000" `shouldBe` True
    it "returns False for even length strings with different halves" $ do
      isRepeatedTwice "1234" `shouldBe` False
      isRepeatedTwice "000123000" `shouldBe` False

  describe "isStringRepeating" $ do
    it "returns True for two identical digits" $ do
      isPeriodic "11" `shouldBe` True
    it "returns True for three identical digits" $ do
      isPeriodic "111" `shouldBe` True
    it "returns True for pairs of identical digits" $ do
      isPeriodic "1212" `shouldBe` True

  describe "toRange" $ do
    it "parses a range string into a tuple" $ do
      toRange "100-200" `shouldBe` (100, 200)
      toRange "001-002" `shouldBe` (1, 2)

  describe "divisors" $ do
    it "lists divisors of a given number" $ do
      divisors 6 `shouldBe` [1, 2, 3, 6]
      divisors 15 `shouldBe` [1, 3, 5, 15]

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      puzzle1 "10-30,1000-1020" `shouldBe` 1043
      puzzle1 "1010-1020" `shouldBe` 1010
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
