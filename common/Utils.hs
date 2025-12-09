module Utils where

import Data.List (tails)

--- Generate all unique pairs from a list
allPairs :: (Ord a) => [a] -> [(a, a)]
allPairs xs = [(a, b) | (a : rest) <- tails xs, b <- rest]

--- Generate all unique pairs except consecutive ones
allNonConsecPairs :: (Ord a) => [a] -> [(a, a)]
allNonConsecPairs xs = [(a, b) | (a : rest) <- tails xs, b <- drop 1 rest]

--- 3D Vector
type Vec3D = (Int, Int, Int)

--- Fast distance calculation between two 3D points, omits the square root for efficiency.
dist3D :: Vec3D -> Vec3D -> Double
dist3D (x1, y1, z1) (x2, y2, z2) =
  fromIntegral $ dx * dx + dy * dy + dz * dz
 where
  dx = x2 - x1
  dy = y2 - y1
  dz = z2 - z1

--- 2D Vector
type Vec2D = (Int, Int)

--- Fast distance calculation between two 2D points, omits the square root for efficiency.
dist2D :: Vec2D -> Vec2D -> Double
dist2D (x1, y1) (x2, y2) =
  fromIntegral $ dx * dx + dy * dy
 where
  dx = x2 - x1
  dy = y2 - y1