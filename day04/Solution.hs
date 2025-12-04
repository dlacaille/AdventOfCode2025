module Solution where

adjacentChars :: [[Char]] -> (Int, Int) -> [Char]
adjacentChars input (x, y) = do
  let hasRowAbove = y > 0
  let hasRowBelow = y < length input - 1
  let hasColLeft = x > 0
  let hasColRight = x < length (head input) - 1
  let startX = if hasColLeft then x - 1 else x
  let endX = if hasColRight then x + 1 else x
  let rowAbove = if hasRowAbove then input !! (y - 1) else []
  let currentRow = input !! y
  let rowBelow = if hasRowBelow then input !! (y + 1) else []
  concat
    [ if hasRowAbove then take (endX - startX + 1) (drop startX rowAbove) else []
    , [currentRow !! (x - 1) | hasColLeft]
    , [currentRow !! (x + 1) | hasColRight]
    , if hasRowBelow then take (endX - startX + 1) (drop startX rowBelow) else []
    ]

charAt :: [[Char]] -> (Int, Int) -> Char
charAt input (x, y) = input !! y !! x

countMatchingAdjacent :: [[Char]] -> (Int, Int) -> Int
countMatchingAdjacent input (x, y) =
  length $ filter (== charAt input (x, y)) $ adjacentChars input (x, y)

positionsOfChar :: [[Char]] -> Char -> [(Int, Int)]
positionsOfChar input char =
  [ (x, y)
  | y <- [0 .. length input - 1]
  , x <- [0 .. length (head input) - 1]
  , charAt input (x, y) == char
  ]

replaceCharAt :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
replaceCharAt [[]] _ _ = [[]]
replaceCharAt input (x, y) newChar =
  let (beforeRow, targetRow : afterRow) = splitAt y input
      (beforeChar, _ : afterChar) = splitAt x targetRow
      newRow = beforeChar ++ [newChar] ++ afterChar
   in beforeRow ++ [newRow] ++ afterRow

puzzle1, puzzle2 :: [[Char]] -> Char -> Int
puzzle1 input =
  length . filter (< 4) . map (countMatchingAdjacent input) . positionsOfChar input
puzzle2 input char = do
  let positions = filter ((< 4) . countMatchingAdjacent input) $ positionsOfChar input char
  if null positions
    then 0
    else
      length positions + puzzle2 (foldl (\inp pos -> replaceCharAt inp pos '.') input positions) char
