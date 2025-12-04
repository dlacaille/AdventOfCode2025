module Main where

import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseLine" $ do
    it "parses line correctly" $ do
      parseLine "L20" `shouldBe` (L, 20)
      parseLine "R15" `shouldBe` (R, 15)
      parseLine "R500" `shouldBe` (R, 500)

  describe "rotateDial" $ do
    it "rotates left correctly" $ do
      rotateDial 50 L 20 `shouldBe` 30
      rotateDial 10 L 20 `shouldBe` 90
    it "rotates right correctly" $ do
      rotateDial 50 R 20 `shouldBe` 70
      rotateDial 90 R 20 `shouldBe` 10
    it "handles zero rotation" $ do
      rotateDial 50 L 0 `shouldBe` 50
      rotateDial 50 R 0 `shouldBe` 50
    it "handles full rotation" $ do
      rotateDial 50 L 100 `shouldBe` 50
      rotateDial 50 R 100 `shouldBe` 50

  describe "countTimesPassedZero" $ do
    it "counts ending on zero when rotating left" $ do
      countTimesPassedZero 10 L 10 `shouldBe` 1
      countTimesPassedZero 1 L 1 `shouldBe` 1
    it "counts passing over zero when rotating left" $ do
      countTimesPassedZero 10 L 20 `shouldBe` 1
      countTimesPassedZero 10 L 11 `shouldBe` 1
    it "counts ending on zero when rotating right" $ do
      countTimesPassedZero 90 R 10 `shouldBe` 1
      countTimesPassedZero 99 R 1 `shouldBe` 1
    it "counts passing over zero when rotating right" $ do
      countTimesPassedZero 90 R 20 `shouldBe` 1
      countTimesPassedZero 90 R 11 `shouldBe` 1
    it "handles no passes over zero when rotating left" $ do
      countTimesPassedZero 10 L 9 `shouldBe` 0
    it "handles no passes over zero when rotating right" $ do
      countTimesPassedZero 90 L 9 `shouldBe` 0
    it "does not count starting on zero" $ do
      countTimesPassedZero 0 L 10 `shouldBe` 0
      countTimesPassedZero 0 R 10 `shouldBe` 0
    it "handles passing over 0 multiple times" $ do
      countTimesPassedZero 50 R 250 `shouldBe` 3
      countTimesPassedZero 50 L 250 `shouldBe` 3

  describe "puzzle1" $ do
    it "computes correct result for sample input" $ do
      puzzle1 [(R, 20), (L, 30), (L, 40)] `shouldBe` 1
      puzzle1 [(R, 20), (L, 30), (L, 40), (R, 5), (L, 5)] `shouldBe` 2
      puzzle1 [(R, 150)] `shouldBe` 1
    it "processes input file" $ do
      readFile "input" >>= pendingWith . show . puzzle1 . map parseLine . lines

  describe "puzzle2" $ do
    it "computes correct result for sample input" $ do
      puzzle2 [(R, 20), (L, 30), (R, 50), (L, 10), (R, 40)] `shouldBe` 1
      puzzle2 [(R, 150)] `shouldBe` 2
      puzzle2 [(L, 50), (L, 1), (L, 200)] `shouldBe` 3
    it "processes input file" $ do
      readFile "input" >>= pendingWith . show . puzzle2 . map parseLine . lines
