module Solution where

import Data.List (find, sortOn, tails)
import Data.Ord (Down (..))

newtype Circuit = Circuit [Position] deriving (Eq, Show)
data Position = Position Int Int Int deriving (Eq, Ord, Show)
data Pair = Pair Position Position

distance :: Position -> Position -> Double
distance (Position x1 y1 z1) (Position x2 y2 z2) =
  sqrt . fromIntegral $ (x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int) + (z2 - z1) ^ (2 :: Int)

connectCircuit :: Pair -> [Circuit] -> [Circuit]
connectCircuit (Pair a b) cs =
  case (find' a, find' b) of
    (Just ca@(Circuit pa), Just cb@(Circuit pb))
      | pa == pb -> cs
      | otherwise -> Circuit (pa ++ pb) : filter (`notElem` [ca, cb]) cs
    (Just ca@(Circuit pa), Nothing) -> Circuit (b : pa) : filter (/= ca) cs
    (Nothing, Just cb@(Circuit pb)) -> Circuit (a : pb) : filter (/= cb) cs
    (Nothing, Nothing) -> Circuit [a, b] : cs
 where
  find' p = find (\(Circuit ps) -> p `elem` ps) cs

puzzle1 :: Int -> [Position] -> IO Int
puzzle1 n xs = do
  let pairs = [(Pair a b, distance a b) | (a : rest) <- tails xs, b <- rest]
      circuits =
        foldl
          (flip connectCircuit)
          [Circuit [p] | p <- xs]
          (map fst $ take n $ sortOn snd pairs)
      sizes = map (\(Circuit ps) -> length ps) circuits
  return . product . take 3 $ sortOn Down sizes

puzzle2 :: [Position] -> IO Int
puzzle2 xs = do
  let pairs = [(Pair a b, distance a b) | (a : rest) <- tails xs, b <- rest]
      go (cs, Nothing) (p, _) =
        let cs' = connectCircuit p cs
         in (cs', if length cs == 2 && length cs' == 1 then Just p else Nothing)
      go acc _ = acc
      Pair (Position x1 _ _) (Position x2 _ _) =
        case foldl go ([Circuit [p] | p <- xs], Nothing) (sortOn snd pairs) of
          (_, Just pair) -> pair
          _ -> error "No solution found"
  return $ x1 * x2