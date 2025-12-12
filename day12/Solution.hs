{-# LANGUAGE BangPatterns #-}

module Solution where

import qualified Data.IntMap.Strict as IntMap

type PresentShape = [[Bool]]
type Region = (Int, Int, [Int])

presentArea :: PresentShape -> Int
presentArea shape = sum $ map (length . filter id) shape

regionArea :: Region -> Int
regionArea (width, height, _) = width * height

puzzle1 :: [PresentShape] -> [Region] -> IO Int
puzzle1 presents regions = do
  let presentMap = IntMap.fromList $ zip [0 :: Int ..] $ map presentArea presents
      fitRegion :: Region -> Bool
      fitRegion region@(_, _, presentCounts) =
        let presentCountsWithIdx = zip [0 :: Int ..] presentCounts
            presentAreasToFit = map (\(idx, count) -> (presentMap IntMap.! idx) * count) presentCountsWithIdx
            totalPresentArea = sum presentAreasToFit
            totalArea = regionArea region
         in totalPresentArea <= totalArea
      fittingRegions = filter fitRegion regions

  return $ length fittingRegions

puzzle2 :: IO Int
puzzle2 = return 0
