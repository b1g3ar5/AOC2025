{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

module Day2(day2) where

import Utils


parse :: String -> [Int]
parse s = concatMap p rs
  where
    rs = splitOn ',' s
    p :: String -> [Int]
    p sr = [(read $ es!!0) .. (read $ es!!1)]
      where
        es = splitOn '-' sr


isValid :: Int -> Int -> Bool
isValid l n = (len `mod` l) /= 0 || not (all (\ix -> ns!!0 == ns!!ix) [1 .. (l-1)])
  where
    sn = show n
    len = length sn
    ns :: [Int]
    ns = read <$> chunksOf (len `div` l) sn


day2 :: IO ()
day2 = do
  ss <- getF parse 2
  putStrLn $ "Day2: part1: " ++ show (sum $ filter (not . isValid 2) ss)
  putStrLn $ "Day2: part1: " ++ show (sum $ nub $ concatMap (\i -> filter (not . isValid i) ss) [2..10])

  return ()

