{- HLINT ignore "Use head" -}

module Day2(day2) where


import Utils
import Data.List.NonEmpty qualified as N

-- I improved on brute force because it was taking 2 seconds

type Range = (Int , Int)


parse :: String -> [Range]
parse s = concatMap p rs
  where
    rs = splitOn ',' s
    p :: String -> [Range]
    p sr
      | llo == lhi = [(read $ N.head es, read $ es N.!! 1)]
      | otherwise = [(read $ N.head es, xx-1), (xx, read $ es N.!! 1)]
      where
        llo = length $ es N.!! 0
        lhi = length $ es N.!! 1
        xx = 10 ^ llo
        es = N.fromList $ splitOn '-' sr


invalids :: Int -> Range -> [Int]
invalids numberOfPieces (lo, hi)
  | (llo `mod` numberOfPieces) /= 0 = []
  | otherwise = filter (\p -> (p>=lo) && (p<=hi)) potentials
  where
    slo = show lo
    shi = show hi
    llo = length slo
    lhi = length shi
    sz = llo ` div` numberOfPieces
    divisor = 10 ^ sz
    multiplier = times (numberOfPieces-2) (\x -> x*divisor + 1) $ divisor+1
    potentials = (* multiplier ) <$> [read $ take sz slo .. if lhi==llo then read (take sz shi) else divisor-1]


day2 :: IO ()
day2 = do
  rs <- getF parse 2

  putStrLn $ "Day2: part1: " ++ show ( sum $ concatMap (invalids 2) rs)
  putStrLn $ "Day2: part2: " ++ show ( sum $ nub $ concatMap (\n -> concatMap (invalids n) rs) [2..10])

  return ()

