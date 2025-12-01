module Day1(day1) where

import Utils


parse :: String -> Int
parse ('L':s) = - (read s)
parse ('R':s) = read s
parse _ = error "I need L or R"


-- There must be a cleaner wa of doing this...
counter :: Int -> Int -> Int
counter pos spin
  | pos==0 = abs (spin `quot` 100) 
  | (pos+spin) == 0 = 1
  | (pos+spin)<0 = 1 + abs ((pos+spin) `quot` 100)
  | otherwise = abs $ (pos+spin) `quot` 100


day1 :: IO ()
day1 = do
  ss <- getLines 1
  --let ss = test
  let spins = parse <$> ss
      code1 =  scanl (\pos spin -> (pos + spin) `mod` 100) 50 spins
      code2 =  foldl (\(count, pos) spin -> (count + counter pos spin, (pos+spin) `mod` 100)) (0,50) spins

  putStrLn $ "Day1: part1: " ++ show (length $ filter (==0) code1)
  putStrLn $ "Day1: part2: " ++ show code2

  return ()
