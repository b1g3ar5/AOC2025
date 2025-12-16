module Day3(day3) where

import Utils ( getLines )


large :: Int -> [Int] -> Int
large size voltages
  | size == 1 = maximum voltages -- we just need one so choose the biggest
  | len == size = read $ concatMap show voltages -- we need all the remaining voltages
  | otherwise = maxv * (10 ^ (size-1)) + large (size-1) (others voltages)
  where
    len = length voltages
    maxv = maximum $ take (len-size+1) voltages
    -- voltages after the maximum
    others [] = error "Can't get here"
    others [p] = [p]
    others (p:ps)
      | p == maxv = ps
      | otherwise = others ps


day3 :: IO ()
day3 = do
  ls <- getLines 3
  let nss = (read . (:[]) <$> ) <$> ls

  putStrLn $ "Day3: part1: " ++ show (sum $ large 2 <$> nss)
  putStrLn $ "Day3: part2: " ++ show (sum $ large 12 <$> nss)

  return ()

