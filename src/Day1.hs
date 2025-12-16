module Day1(day1) where

import Utils


parse :: String -> Int
parse ('L':s) = - (read s)
parse ('R':s) = read s
parse _ = error "I need L or R"


-- The difference between divMod and quotRem on negative numbers (no difference for positive)
-- (-23) `divMod` 10 = (-3, 7) -- chooses the div closer to -inf, mod always positive
-- (-23) `quotRem` 10 = (-2, -3) -- chooses the quot closer to 0
                                 -- so it's symmetrical around zero, rem is negative

counter :: Int -> Int -> Int
counter pos spin
  | pos == 0 = abs (spin `quot` 100) -- if we start at 0 there's no extra zero
  | (pos+spin) == 0 = 1 -- if we go back to 0, just a zero
  | (pos+spin) < 0 = 1 + abs ((pos+spin) `quot` 100) -- if we go down there's an extra zero
  | otherwise = abs $ (pos+spin) `div` 100 -- quot or div on this one


day1 :: IO ()
day1 = do
  ss <- getLines 1
  let spins = parse <$> ss
      code1 =  length $ filter (==0) $ scanl (\p s -> (p+s) `mod` 100) 50 spins
      code2 =  foldl (\(count, pos) spin -> (count + counter pos spin, (pos+spin) `mod` 100)) (0,50) spins

  putStrLn $ "Day1: part1: " ++ show code1
  putStrLn $ "Day1: part2: " ++ show (fst code2)

  return ()
