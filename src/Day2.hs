{- HLINT ignore "Use head" -}
module Day2(day2) where


import Utils (getF, splitOn, times)
import Data.List.NonEmpty qualified as N
import Data.Set (Set)
import Data.Set qualified as S


type Range = (Int , Int)


parse :: String -> [Range]
parse s = concatMap p rs
  where
    rs = splitOn ',' s
    p :: String -> [Range]
    p sr
      | llo == lhi = [(read $ N.head es, read $ es N.!! 1)]
      | otherwise = [(read $ N.head es, xx-1), (xx, read $ es N.!! 1)] -- split into 2 sizes
      where
        llo = length $ es N.!! 0
        lhi = length $ es N.!! 1
        xx = 10 ^ llo
        es = N.fromList $ splitOn '-' sr


invalids :: Int -> Range -> Set Int
invalids numberOfPieces (lo, hi)
  | (llo `mod` numberOfPieces) /= 0 = S.empty -- numberOfPieces is not a factor of length
  | otherwise = S.filter (\p -> (p>=lo) && (p<=hi)) potentials
  where
    slo = show lo
    llo = length slo
    sz = llo ` div` numberOfPieces
    divisor = 10 ^ sz
    multiplier = times (numberOfPieces-2) ((+1) . (*divisor)) $ divisor+1
    -- takes the chars from the start  of lo and hi makes a range 
    -- and repeats them sz times (ie. * multiplier)
    potentials = S.fromList $ (* multiplier) <$> [read (take sz slo) .. read (take sz $ show hi)]


day2 :: IO ()
day2 = do
  rs <- getF parse 2

  putStrLn $ "Day2: part1: " ++ show ( sum $ S.unions $ invalids 2 <$> rs)
  putStrLn $ "Day2: part2: " ++ show ( sum $ S.unions $ (\n -> S.unions $ invalids n <$> rs) <$> [2..10])

  return ()

