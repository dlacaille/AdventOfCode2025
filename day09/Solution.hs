module Solution where

import Data.List (nub, sortOn)
import Data.Ord (Down (Down))
import Utils

--- Get all points of a rectangle defined by two points
rectPoints :: Vec2D -> Vec2D -> [Vec2D]
rectPoints (x1, y1) (x2, y2) =
  [ (x, y)
  | x <- [min x1 x2 .. max x1 x2]
  , y <- [min y1 y2 .. max y1 y2]
  ]

--- Get all points on boundary and inside polygon (axis-aligned)
polyPoints :: [(Int, Int)] -> [(Int, Int)]
polyPoints poly = nub $ boundary ++ interior
 where
  polyEdges = zip poly (tail poly ++ [head poly])
  boundary = concatMap axisLine polyEdges
  interior =
    filter
      (pointInPoly polyEdges)
      [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  (minX, maxX, minY, maxY) =
    let (xs, ys) = unzip poly
     in (minimum xs, maximum xs, minimum ys, maximum ys)

  axisLine ((x0, y0), (x1, y1))
    | x0 == x1 = [(x0, y) | y <- [min y0 y1 .. max y0 y1]]
    | otherwise = [(x, y0) | x <- [min x0 x1 .. max x0 x1]]

  pointInPoly edges (px, py) = odd $ length $ filter crossesRay edges
   where
    crossesRay ((x1, y1), (x2, y2))
      | x1 /= x2 = False
      | y1 <= py && y2 > py = px < x1
      | y2 <= py && y1 > py = px < x1
      | otherwise = False

--- Print all points of a polygon in a grid for debugging purposes
printPoly :: [Vec2D] -> [Vec2D] -> [Vec2D] -> IO ()
printPoly hes xes oes = mapM_ putStrLn rows
 where
  rows =
    [ [ renderCell x y
      | x <- [0 .. maxX + 2]
      ]
    | y <- [0 .. maxY + 1]
    ]
  (maxX, maxY) =
    let (xs, ys) = unzip xes
     in (maximum xs, maximum ys)

  renderCell x y
    | (x, y) `elem` oes = 'O'
    | (x, y) `elem` hes = '#'
    | (x, y) `elem` xes = 'X'
    | otherwise = '.'

--- Area of a rectangle between two points
areaRect2D :: Vec2D -> Vec2D -> Int
areaRect2D (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

puzzle1 :: [Vec2D] -> IO Int
puzzle1 xs = do
  let areas = map (\(a, b) -> ((a, b), areaRect2D a b)) (allNonConsecPairs xs)
      sortedAreas = sortOn (Down . snd) areas
  return $ snd $ head sortedAreas

puzzle2 :: [Vec2D] -> IO Int
puzzle2 xs = do
  let poly = polyPoints xs
      areas = map (\(a, b) -> ((a, b), areaRect2D a b)) (allNonConsecPairs xs)
      sortedAreas = sortOn (Down . snd) areas
      filteredAreas = filter (\((a, b), _) -> all (`elem` poly) (rectPoints a b)) sortedAreas
  return $ snd $ head filteredAreas