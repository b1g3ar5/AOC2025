module Day12(day12) where

import Utils


parse :: [String] -> [((Int, Int), [Int])]
parse s = parseRegion <$> rs
  where
    ps = splitOn "" s
    rs = concat $ drop 6 ps
    parseRegion :: String -> ((Int, Int), [Int])
    parseRegion  l = ((read $ take 2 $ ws!!0, read $ drop 3 $ init $ ws!!0), read <$> tail ws)
      where
        ws = words l

day12 :: IO ()
day12 = do
  ss <- getLines 12
  let out = parse ss


  putStrLn $ "Day12: part1: " ++ show (length $ filter (\((x,y), xs) -> x*y> 8*sum xs) out)
  --putStrLn $ "Day12: part2: " ++ show ss

  return ()
