module Solution where

import Data.List (find, sortOn, tails)
import Data.Ord (Down (..))

type Pos = (Int, Int, Int)

distance :: Pos -> Pos -> Double
distance (x1, y1, z1) (x2, y2, z2) =
  sqrt . fromIntegral $ dx * dx + dy * dy + dz * dz
 where
  dx = x2 - x1
  dy = y2 - y1
  dz = z2 - z1

connect :: (Pos, Pos) -> [[Pos]] -> [[Pos]]
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

allPairDistances :: [Pos] -> [((Pos, Pos), Double)]
allPairDistances xs = sortOn snd [((a, b), distance a b) | (a : rest) <- tails xs, b <- rest]

puzzle1 :: Int -> [Pos] -> Int
puzzle1 n xs =
  let circuits = foldl (flip connect) [[p] | p <- xs] (map fst $ take n $ allPairDistances xs)
   in product . take 3 $ sortOn Down (map length circuits)

puzzle2 :: [Pos] -> Int
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