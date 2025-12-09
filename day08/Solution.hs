module Solution where

import Data.List (find, sortOn)
import Data.Ord (Down (..))
import Utils

connect :: (Vec3D, Vec3D) -> [[Vec3D]] -> [[Vec3D]]
connect (a, b) cs =
  case (find' a, find' b) of
    (Just pa, Just pb)
      | pa == pb -> cs
      | otherwise -> (pa ++ pb) : filter (`notElem` [pa, pb]) cs
    (Just pa, Nothing) -> (b : pa) : filter (/= pa) cs
    (Nothing, Just pb) -> (a : pb) : filter (/= pb) cs
    (Nothing, Nothing) -> [a, b] : cs
 where
  find' p = find (p `elem`) cs

allPairDistances :: [Vec3D] -> [((Vec3D, Vec3D), Double)]
allPairDistances xs = map (\(a, b) -> ((a, b), dist3D a b)) (allPairs xs)

puzzle1 :: Int -> [Vec3D] -> Int
puzzle1 n xs =
  let circuits = foldl (flip connect) [[p] | p <- xs] (map fst $ take n $ allPairDistances xs)
   in product . take 3 $ sortOn Down (map length circuits)

puzzle2 :: [Vec3D] -> Int
puzzle2 xs =
  let go (cs, Nothing) (p, _) =
        let cs' = connect p cs
         in (cs', if length cs == 2 && length cs' == 1 then Just p else Nothing)
      go acc _ = acc
      ((x1, _, _), (x2, _, _)) =
        case foldl go ([[p] | p <- xs], Nothing) (allPairDistances xs) of
          (_, Just pair) -> pair
          _ -> error "No solution found"
   in x1 * x2