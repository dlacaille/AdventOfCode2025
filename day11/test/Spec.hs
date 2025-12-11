module Main where

import qualified Data.HashMap.Strict as HashMap
import Solution
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "puzzle1" $ do
    it "solves the puzzle for given input" $ do
      let connections =
            HashMap.fromList
              [ ("aaa", ["you", "hhh"])
              , ("you", ["bbb", "ccc"])
              , ("bbb", ["ddd", "eee"])
              , ("ccc", ["ddd", "eee", "fff"])
              , ("ddd", ["ggg"])
              , ("eee", ["out"])
              , ("fff", ["out"])
              , ("ggg", ["out"])
              , ("hhh", ["ccc", "fff", "iii"])
              , ("iii", ["out"])
              ]
      puzzle1 connections `shouldReturn` 5

    it "processes input file" $ do
      contents <- readFile "input"
      let graph = HashMap.fromListWith (++) $ map parseLine (lines contents)
          parseLine line = case words line of
            (node : neighbors) -> (init node, neighbors)
            [] -> ("", [])
      results <- puzzle1 graph
      pendingWith $ show results

  describe "puzzle2" $ do
    it "solves the puzzle for given input" $ do
      let connections =
            HashMap.fromList
              [ ("svr", ["aaa", "bbb"])
              , ("aaa", ["fft"])
              , ("fft", ["ccc"])
              , ("bbb", ["tty"])
              , ("tty", ["ccc"])
              , ("ccc", ["ddd", "eee"])
              , ("ddd", ["hub"])
              , ("hub", ["fff"])
              , ("eee", ["dac"])
              , ("dac", ["fff"])
              , ("fff", ["ggg", "hhh"])
              , ("ggg", ["out"])
              , ("hhh", ["out"])
              ]
      puzzle2 connections `shouldReturn` 2

    it "processes input file" $ do
      contents <- readFile "input"
      let graph = HashMap.fromListWith (++) $ map parseLine (lines contents)
          parseLine line = case words line of
            (node : neighbors) -> (init node, neighbors)
            [] -> ("", [])
      results <- puzzle2 graph
      pendingWith $ show results