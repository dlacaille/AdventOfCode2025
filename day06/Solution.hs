module Solution where

getColumnWidths :: [Char] -> [Int]
getColumnWidths [] = []
getColumnWidths ls = tail $ foldr countWidths [] ls
 where
  countWidths ' ' [] = [1]
  countWidths ' ' (x : xs) = (x + 1) : xs
  countWidths _ [] = [1]
  countWidths _ (x : xs) = 0 : (x + 1) : xs

splitColumnsOnWidths :: String -> [Int] -> [String]
splitColumnsOnWidths _ [] = []
splitColumnsOnWidths str (w : ws) = let (col, rest) = splitAt w str in col : splitColumnsOnWidths rest ws

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

toOperation :: (Num a) => Char -> a -> a -> a
toOperation '+' = (+)
toOperation '*' = (*)
toOperation _ = error "Unsupported operation"

puzzle1 :: [[Int]] -> [Char] -> Int
puzzle1 columns operations =
  sum $
    zipWith
      ( \col op ->
          ( case col of
              [] -> 0
              (x : xs) -> foldl (toOperation op) x xs
          )
      )
      columns
      operations

puzzle2 :: [String] -> Int
puzzle2 input = do
  let operations = reverse $ map head $ words $ last input
  let columnWidths = getColumnWidths (last input)
  let columns = map (reverse . transpose) $ reverse $ transpose $ map (`splitColumnsOnWidths` columnWidths) (init input)
  let parsed = map (map read . filter (not . null) . map (filter (/= ' '))) columns :: [[Int]]
  puzzle1 parsed operations