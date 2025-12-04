module Solution where

import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V

type CharGrid = Vector (Vector Char)

charAt :: Int -> Int -> CharGrid -> Maybe Char
charAt x y grid = grid !? y >>= (!? x)

gridFromLines :: [String] -> CharGrid
gridFromLines = V.fromList . map V.fromList

adjacentPositions :: Int -> Int -> [(Int, Int)]
adjacentPositions x y =
  [ (x + dx, y + dy)
  | dy <- [-1, 0, 1]
  , dx <- [-1, 0, 1]
  , (dx, dy) /= (0, 0)
  ]

adjacentChars :: Int -> Int -> CharGrid -> String
adjacentChars x y grid = mapMaybe (\(px, py) -> charAt px py grid) (adjacentPositions x y)

countMatchingAdjacent :: Int -> Int -> CharGrid -> Int
countMatchingAdjacent x y grid =
  length $ filter (== charAt x y grid) $ map Just $ adjacentChars x y grid

gridSize :: CharGrid -> (Int, Int)
gridSize grid
  | V.null grid = (0, 0)
  | otherwise = (V.length (V.head grid), V.length grid)

positionsOfChar :: Char -> CharGrid -> [(Int, Int)]
positionsOfChar char grid =
  [ (x, y)
  | y <- [0 .. height - 1]
  , x <- [0 .. width - 1]
  , charAt x y grid == Just char
  ]
 where
  (width, height) = gridSize grid

replaceCharAt :: Int -> Int -> Char -> CharGrid -> CharGrid
replaceCharAt x y newChar grid = grid V.// [(y, row V.// [(x, newChar)])]
 where
  row = grid V.! y

puzzle1, puzzle2 :: Char -> [String] -> Int
puzzle1 c input = length $ filter (< 4) $ map (uncurry countMatchingAdjacent') positions
 where
  grid = gridFromLines input
  positions = positionsOfChar c grid
  countMatchingAdjacent' x y = countMatchingAdjacent x y grid
puzzle2 c input = go (gridFromLines input)
 where
  go grid
    | null positions = 0
    | otherwise = length positions + go newGrid
   where
    positions = filter (\(x, y) -> countMatchingAdjacent x y grid < 4) $ positionsOfChar c grid
    newGrid = foldr (\(x, y) g -> replaceCharAt x y '.' g) grid positions
