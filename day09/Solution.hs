module Solution where

import Data.List (find, sortOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Utils

-- | Area of a rectangle between two points
areaRect2D :: Vec2D -> Vec2D -> Int
areaRect2D (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

-- | Check if a rectangle doesn't cross any edges
rectIsValid :: [(Vec2D, Vec2D)] -> [(Vec2D, Vec2D)] -> Vec2D -> Vec2D -> Bool
rectIsValid hEdges vEdges (x1, y1) (x2, y2) =
  let minX = min x1 x2
      maxX = max x1 x2
      minY = min y1 y2
      maxY = max y1 y2
      crossesHorizontal ((ex1, ey1), (ex2, _)) =
        let edgeY = ey1
            edgeMinX = min ex1 ex2
            edgeMaxX = max ex1 ex2
         in edgeY > minY && edgeY < maxY && edgeMinX < maxX && edgeMaxX > minX
      crossesVertical ((ex1, ey1), (_, ey2)) =
        let edgeX = ex1
            edgeMinY = min ey1 ey2
            edgeMaxY = max ey1 ey2
         in edgeX > minX && edgeX < maxX && edgeMinY < maxY && edgeMaxY > minY
   in not (any crossesHorizontal hEdges) && not (any crossesVertical vEdges)

puzzle1 :: [Vec2D] -> IO Int
puzzle1 xs = do
  let areas = map (\(a, b) -> ((a, b), areaRect2D a b)) (allNonConsecPairs xs)
      sortedAreas = sortOn (Down . snd) areas
  return $ snd $ head sortedAreas

puzzle2 :: [Vec2D] -> IO Int
puzzle2 xs = do
  let edges = zip xs (tail xs ++ [head xs])
      hEdges = [e | e@((_, y1), (_, y2)) <- edges, y1 == y2]
      vEdges = [e | e@((x1, _), (x2, _)) <- edges, x1 == x2]
      areas = map (\(a, b) -> ((a, b), areaRect2D a b)) (allNonConsecPairs xs)
      sortedAreas = sortOn (Down . snd) areas
  return $ snd $ fromJust $ find (\((a, b), _) -> rectIsValid hEdges vEdges a b) sortedAreas