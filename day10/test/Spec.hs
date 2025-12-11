module Main where

import Data.List.Split (splitOn)
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "solve" $ do
    it "solves a simple problem" $ do
      let desiredState = [True]
          initialState = replicate (length desiredState) False
          buttons = [[0]]
      solve [] not buttonsAfter initialState desiredState buttons `shouldBe` [[[0]]]
    it "selects the right button" $ do
      let desiredState = [False, True]
          initialState = replicate (length desiredState) False
          buttons = [[0], [1]]
      solve [] not buttonsAfter initialState desiredState buttons `shouldBe` [[[1]]]
    it "selects multiple buttons" $ do
      let desiredState = [False, True, False]
          initialState = replicate (length desiredState) False
          buttons = [[2], [0, 1], [0]]
      solve [] not buttonsAfter initialState desiredState buttons `shouldBe` [[[0, 1], [0]]]
    it "uses initial state" $ do
      let desiredState = [False, True, False]
          initialState = [True, False, False]
          buttons = [[2], [0, 1]]
      solve [] not buttonsAfter initialState desiredState buttons `shouldBe` [[[0, 1]]]
    it "uses already pressed buttons" $ do
      let desiredState = [False, True, True, False]
          initialState = [True, True, False, False]
          pressedButtons = [[0, 1]]
          buttons = [[3], [1, 3], [2, 3], [0, 2]]
      solve pressedButtons not buttonsAfter initialState desiredState buttons `shouldBe` [[[0, 2], [0, 1]]]

  describe "start" $ do
    it "starts the first machine" $ do
      let desiredState = [False, True, True, False]
          buttons = [[3], [1, 3], [2, 3], [0, 2], [0, 1]]
          machine = (desiredState, buttons)
      start machine not buttonsAfter False `shouldBe` Just [[0, 2], [0, 1]]
    it "starts the second machine" $ do
      let desiredState = [False, False, False, True, False]
          buttons = [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]]
          machine = (desiredState, buttons)
      start machine not buttonsAfter False `shouldBe` Just [[1, 2, 3, 4], [0, 4], [0, 1, 2]]
    it "starts the third machine" $ do
      let desiredState = [False, True, True, True, False, True]
          buttons = [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]]
          machine = (desiredState, buttons)
      start machine not buttonsAfter False `shouldBe` Just [[0, 3, 4], [0, 1, 2, 4, 5]]

  describe "checkState" $ do
    it "filters buttons that do not exceed desired state" $ do
      let desiredState = [2, 1]
          state = [2, 0]
          buttons = [[0], [1]]
          result = checkState [] state desiredState buttons
      result `shouldBe` [[1]]

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
      let parseMachine :: String -> ([Bool], [Button])
          parseMachine str =
            case words str of
              [] -> ([], [])
              (indicatorsStr : buttonsStr) ->
                let indicators = map (== '#') (init $ tail indicatorsStr)
                    buttons = map (map read . splitOn [','] . tail . init) (init buttonsStr)
                 in (indicators, buttons)
          machines = map parseMachine $ lines contents
      result <- puzzle1 machines
      pendingWith (show result)

  focus $ describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let input =
            [ ([3, 5, 4, 7], [[3], [1, 3], [2, 3], [0, 2], [0, 1]])
            , ([7, 5, 12, 7, 2], [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]])
            , ([10, 11, 11, 5, 10, 5], [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]])
            ]
      puzzle2 input `shouldReturn` 33

    it "processes input file" $ do
      contents <- readFile "input"
      let parseMachine :: String -> ([Int], [Button])
          parseMachine str =
            case words str of
              [] -> ([], [])
              (_ : buttonsStr) ->
                let joltages = map read (splitOn [','] $ tail $ init $ last buttonsStr)
                    buttons = map (map read . splitOn [','] . tail . init) (init buttonsStr)
                 in (joltages, buttons)
          machines = map parseMachine $ lines contents
      result <- puzzle2 machines
      pendingWith (show result)