module Spec where

import Test.Hspec
import Utils

spec :: IO ()
spec = hspec $ do
  describe "parseLine" $ do
    it "parses line correctly" $ do
      parseLine "L 20" `shouldBe` ('L', 20)
      parseLine "R 15" `shouldBe` ('R', 15)

  describe "rotateDial" $ do
    it "rotates left correctly" $ do
      rotateDial 50 'L' 20 `shouldBe` 30
      rotateDial 10 'L' 20 `shouldBe` 90
    it "rotates right correctly" $ do
      rotateDial 50 'R' 20 `shouldBe` 70
      rotateDial 90 'R' 20 `shouldBe` 10
    it "handles invalid direction" $ do
      rotateDial 50 'X' 20 `shouldBe` 50
    it "handles zero rotation" $ do
      rotateDial 50 'L' 0 `shouldBe` 50
      rotateDial 50 'R' 0 `shouldBe` 50
    it "handles full rotation" $ do
      rotateDial 50 'L' 100 `shouldBe` 50
      rotateDial 50 'R' 100 `shouldBe` 50

  describe "countTimesPassedZero" $ do
    it "counts ending on zero when rotating left" $ do
      countTimesPassedZero 10 'L' 10 `shouldBe` 1
      countTimesPassedZero 1 'L' 1 `shouldBe` 1
    it "counts passing over zero when rotating left" $ do
      countTimesPassedZero 10 'L' 20 `shouldBe` 1
      countTimesPassedZero 10 'L' 11 `shouldBe` 1
    it "counts ending on zero when rotating right" $ do
      countTimesPassedZero 90 'R' 10 `shouldBe` 1
      countTimesPassedZero 99 'R' 1 `shouldBe` 1
    it "counts passing over zero when rotating right" $ do
      countTimesPassedZero 90 'R' 20 `shouldBe` 1
      countTimesPassedZero 90 'R' 11 `shouldBe` 1
    it "handles no passes over zero when rotating left" $ do
      countTimesPassedZero 10 'L' 9 `shouldBe` 0
    it "handles no passes over zero when rotating right" $ do
      countTimesPassedZero 90 'L' 9 `shouldBe` 0
    it "does not count starting on zero" $ do
      countTimesPassedZero 0 'L' 10 `shouldBe` 0
      countTimesPassedZero 0 'R' 10 `shouldBe` 0
    it "handles passing over 0 multiple times" $ do
      countTimesPassedZero 50 'R' 250 `shouldBe` 3
      countTimesPassedZero 50 'L' 250 `shouldBe` 3
