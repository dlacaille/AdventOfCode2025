module Main where

import Data.List.Split (splitOn)
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "toggleButton" $ do
    it "toggles the correct indicators" $ do
      let button = [0, 2]
          initialState = [False, False, False]
      toggleButton button initialState `shouldBe` [True, False, True]
    it "toggles multiple times" $ do
      let button = [1, 2]
          initialState = [True, True, False]
      toggleButton button initialState `shouldBe` [True, False, True]

  describe "solve" $ do
    it "solves a simple problem" $ do
      let desiredState = [True]
          initialState = replicate (length desiredState) False
          buttons = [[0]]
      solve [] initialState desiredState buttons `shouldBe` [[[0]]]
    it "selects the right button" $ do
      let desiredState = [False, True]
          initialState = replicate (length desiredState) False
          buttons = [[0], [1]]
      solve [] initialState desiredState buttons `shouldBe` [[[1]]]
    it "selects multiple buttons" $ do
      let desiredState = [False, True, False]
          initialState = replicate (length desiredState) False
          buttons = [[2], [0, 1], [0]]
      solve [] initialState desiredState buttons `shouldBe` [[[0, 1], [0]]]
    it "uses initial state" $ do
      let desiredState = [False, True, False]
          initialState = [True, False, False]
          buttons = [[2], [0, 1]]
      solve [] initialState desiredState buttons `shouldBe` [[[0, 1]]]
    it "uses already pressed buttons" $ do
      let desiredState = [False, True, True, False]
          initialState = [True, True, False, False]
          pressedButtons = [[0, 1]]
          buttons = [[3], [1, 3], [2, 3], [0, 2]]
      solve pressedButtons initialState desiredState buttons `shouldBe` [[[0, 2], [0, 1]]]

  describe "start" $ do
    it "starts the first machine" $ do
      let desiredState = [False, True, True, False]
          buttons = [[3], [1, 3], [2, 3], [0, 2], [0, 1]]
          machine = Machine desiredState buttons
      start machine `shouldBe` Just [[0, 2], [0, 1]]
    it "starts the second machine" $ do
      let desiredState = [False, False, False, True, False]
          buttons = [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]]
          machine = Machine desiredState buttons
      start machine `shouldBe` Just [[1, 2, 3, 4], [0, 4], [0, 1, 2]]
    it "starts the third machine" $ do
      let desiredState = [False, True, True, True, False, True]
          buttons = [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]]
          machine = Machine desiredState buttons
      start machine `shouldBe` Just [[0, 3, 4], [0, 1, 2, 4, 5]]

  describe "puzzle1" $ do
    it "sums all results" $ do
      let input =
            [ Machine [False, True, True, False] [[3], [1, 3], [2, 3], [0, 2], [0, 1]]
            , Machine [False, False, False, True, False] [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]]
            , Machine [False, True, True, True, False, True] [[0, 1, 2, 3, 4], [0, 3, 4], [0, 1, 2, 4, 5], [1, 2]]
            ]
      puzzle1 input `shouldReturn` 7

    it "processes input file" $ do
      contents <- readFile "input"
      let parseMachine :: String -> Machine
          parseMachine str =
            case words str of
              [] -> Machine [] []
              (indicatorsStr : buttonsStr) ->
                let indicators = map (== '#') (init $ tail indicatorsStr)
                    buttons = map (map read . splitOn [','] . tail . init) (init buttonsStr)
                 in Machine indicators buttons
          machines = map parseMachine $ lines contents
      print machines
      result <- puzzle1 machines
      pendingWith (show result)

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      pending

    it "processes input file" $ do
      pending