module Day5(day5) where

import Utils (sort, getLines, splitOn)
import Data.List.NonEmpty qualified as N


type Range = (Int, Int)


parseRange :: String -> Range
parseRange es = (read (N.head ps), read (ps N.!! 1))
  where
    ps = N.fromList $ splitOn '-' es


member :: Int -> Range -> Bool
member x (lo, hi) = x>=lo && x <= hi


-- Sort the ranges
parse :: [String] -> (N.NonEmpty Range, [Int])
parse ss = (N.fromList $ sort $ parseRange <$> N.head ps,  read <$> ps N.!! 1)
  where
    ps :: N.NonEmpty [String]
    ps = N.fromList $ splitOn "" ss


-- Sort ou overlpas - input ranges must be sorted
addRange :: Range -> [Range] -> [Range]
addRange next [] = [next]
addRange (nlo, nhi) ((lo, hi):rs)
  | lo>(nhi+1) = (nlo, nhi) : addRange (lo, hi) rs
  | otherwise = addRange (nlo, max nhi hi) rs


day5 :: IO ()
day5 = do
  ss <- getLines 5
  let (fresh, available) = parse ss

  putStrLn $ "Day5: part1: " ++ show (length $ filter (\x -> any (member x) fresh) available)
  putStrLn $ "Day5: part2: " ++ show (sum $ (\(l,h) -> h-l+1) <$> foldr addRange [N.head fresh] (N.tail fresh))

  return ()


