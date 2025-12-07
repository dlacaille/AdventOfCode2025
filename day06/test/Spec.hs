module Main where

import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "transpose" $ do
    it "transposes a matrix" $ do
      let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Integer]]
      transpose matrix `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
    it "transposes text" $ do
      let matrix = ["64 ", "23 ", "314"]
      (reverse . transpose) matrix `shouldBe` ["  4", "431", "623"]

  describe "getColumnWidths" $ do
    it "finds width of columns" $ do
      let lst = "*    *  * **"
      getColumnWidths lst `shouldBe` [5, 3, 2, 2]

  describe "splitColumnsOnWidths" $ do
    it "splits string into columns based on widths" $ do
      let str = "123 328  51 64"
      let widths = [4, 4, 3, 3]
      splitColumnsOnWidths str widths `shouldBe` ["123 ", "328 ", " 51", " 64"]

  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let columns =
            [ [123, 45, 6]
            , [328, 64, 98]
            , [51, 387, 215]
            , [64, 23, 314]
            ]
      let operations = ['*', '+', '*', '+']
      puzzle1 columns operations `shouldBe` 4277556
    it "processes input file" $ do
      contents <- readFile "input"
      let input = lines contents
      let columns = map (map read) $ transpose $ map words $ init input
      let operations = map head $ words $ last input
      let result = puzzle1 columns operations
      pendingWith $ show result

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ "123 328  51 64 "
            , " 45 64  387 23 "
            , "  6 98  215 314"
            , "*   +   *   +  "
            ]
      puzzle2 input `shouldBe` 3263827
    it "processes input file" $ do
      contents <- readFile "input"
      let input = lines contents
      let result = puzzle2 input
      pendingWith $ show result
