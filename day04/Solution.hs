module Solution where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type CharGrid = IntMap (IntMap Char)

charAt :: Int -> Int -> CharGrid -> Maybe Char
charAt x y input = IntMap.lookup y input >>= IntMap.lookup x

gridFromLines :: [String] -> CharGrid
gridFromLines xs =
  IntMap.fromList
    [ (y, IntMap.fromList [(x, c) | (x, c) <- zip [0 ..] line])
    | (y, line) <- zip [0 ..] xs
    ]

adjacentChars :: Int -> Int -> CharGrid -> [Char]
adjacentChars x y input =
  let aboveRow = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
      targetRow = [(x - 1, y), (x + 1, y)]
      belowRow = [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
      positions = aboveRow ++ targetRow ++ belowRow
   in [c | (row, col) <- positions, Just c <- [charAt row col input]]

countMatchingAdjacent :: Int -> Int -> CharGrid -> Int
countMatchingAdjacent x y input =
  length $ filter (\a -> Just a == charAt x y input) $ adjacentChars x y input

gridSize :: IntMap (IntMap a) -> (Int, Int)
gridSize input =
  let width = length (head $ IntMap.elems input)
      height = length input
   in (width, height)

positionsOfChar :: Char -> CharGrid -> [(Int, Int)]
positionsOfChar char input =
  let (width, height) = gridSize input
   in [ (x, y)
      | y <- [0 .. height - 1]
      , x <- [0 .. width - 1]
      , charAt x y input == Just char
      ]

replaceCharAt :: Int -> Int -> Char -> CharGrid -> CharGrid
replaceCharAt x y newChar =
  IntMap.adjust (IntMap.insert x newChar) y

puzzle1, puzzle2 :: Char -> CharGrid -> IO Int
puzzle1 c input = return $ length $ filter (< 4) $ map (\(x, y) -> countMatchingAdjacent x y input) $ positionsOfChar c input
puzzle2 c input = do
  let positions = filter ((< 4) . \(x, y) -> countMatchingAdjacent x y input) $ positionsOfChar c input
  if null positions
    then return 0
    else do
      let newInput = foldl (\i (x, y) -> replaceCharAt x y '.' i) input positions
      x <- puzzle2 c newInput
      return $ length positions + x
