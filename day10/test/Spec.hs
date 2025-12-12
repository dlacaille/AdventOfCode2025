module Main where

import Data.List.Split (splitOn)
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "solve1" $ do
    it "solves a simple problem" $ do
      let desiredState = [True]
          buttons = [[0]]
      solve1 [] desiredState buttons `shouldBe` [[[0]]]
    it "selects the right button" $ do
      let desiredState = [False, True]
          buttons = [[0], [1]]
      solve1 [] desiredState buttons `shouldBe` [[[1]]]
    it "selects multiple buttons" $ do
      let desiredState = [False, True, False]
          buttons = [[2], [0, 1], [0]]
      solve1 [] desiredState buttons `shouldBe` [[[0, 1], [0]]]
    it "works with multiple button options" $ do
      let desiredState = [True, False]
          buttons = [[0], [1], [0, 1]]
      solve1 [] desiredState buttons `shouldBe` [[[0]]]
    it "uses already pressed buttons" $ do
      let desiredState = [False, True, True, False]
          pressedButtons = [[0, 1]]
          buttons = [[3], [1, 3], [2, 3], [0, 2]]
      solve1 pressedButtons desiredState buttons `shouldBe` [[[0, 2], [0, 1]]]

  describe "start1" $ do
    it "starts the first machine" $ do
      let desiredState = [False, True, True, False]
          buttons = [[3], [1, 3], [2, 3], [0, 2], [0, 1]]
          machine = (desiredState, buttons)
      start1 machine `shouldBe` Just [[2, 3], [1, 3]]
    it "starts the second machine" $ do
      let desiredState = [False, False, False, True, False]
          buttons = [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]]
          machine = (desiredState, buttons)
      start1 machine `shouldBe` Just [[1, 2, 3, 4], [0, 4], [0, 1, 2]]
    it "starts the third machine" $ do
      let desiredState = [False, True, True, True, False, True]
          buttons = [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]]
          machine = (desiredState, buttons)
      start1 machine `shouldBe` Just [[0, 3, 4], [0, 1, 2, 4, 5]]

  describe "puzzle1" $ do
    it "sums all results" $ do
      let input =
            [ ([False, True, True, False], [[3], [1, 3], [2, 3], [0, 2], [0, 1]])
            , ([False, False, False, True, False], [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]])
            , ([False, True, True, True, False, True], [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]])
            ]
      puzzle1 input `shouldReturn` 7

    it "processes input file" $ do
      contents <- readFile "input"
      let parseMachine :: String -> ([Bool], [[Int]])
          parseMachine str =
            case words str of
              [] -> ([], [])
              (indicatorsStr : buttonsStr) ->
                let indicators = map (== '#') (init $ tail indicatorsStr)
                    buttons = map (map read . splitOn [','] . tail . init) (init buttonsStr)
                 in (indicators, buttons)
          machines = map parseMachine $ lines contents
      print machines
      result <- puzzle1 machines
      pendingWith $ show result

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ ([3, 5, 4, 7], [[3], [1, 3], [2, 3], [0, 2], [0, 1]])
            , ([7, 5, 12, 7, 2], [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]])
            , ([10, 11, 11, 5, 10, 5], [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]])
            ]
      puzzle2 input `shouldReturn` 33

    it "solves a slow machine" $ do
      let input =
            [
              ( [36, 74, 63, 66, 60, 64, 66, 59, 67, 31]
              , [[3, 5, 6, 7], [0, 3, 4, 5, 6, 8, 9], [1, 3, 8], [0, 2, 3, 4, 5, 6, 7, 8, 9], [0, 1, 3, 4, 5, 6, 8], [1, 7], [0, 1, 2, 4, 5, 7, 9], [1, 2, 3, 4, 6, 7, 8, 9], [1, 2, 4, 5], [2, 6, 8], [0, 1, 2, 3, 4, 5, 7, 9], [0, 7], [1, 3, 5, 8]]
              )
            ]
      result <- puzzle2 input
      pendingWith $ show result

    it "processes input file" $ do
      contents <- readFile "input"
      let parseMachine :: String -> ([Int], [[Int]])
          parseMachine str =
            case words str of
              [] -> ([], [])
              (_ : buttonsStr) ->
                let joltages = map read (splitOn [','] $ tail $ init $ last buttonsStr)
                    buttons = map (map read . splitOn [','] . tail . init) (init buttonsStr)
                 in (joltages, buttons)
          machines = map parseMachine $ lines contents
      result <- puzzle2 machines
      pendingWith $ show result