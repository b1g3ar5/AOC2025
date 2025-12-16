{- HLINT ignore "Use head" -}

module Day6(day6) where


import Utils (transpose, getF, splitOn)


parse1 :: [String] -> Int
parse1 s = sum $ zipWith (\op ns -> if op == '+' then sum ns else product ns) syms (transpose cs)
  where
    cs :: [[Int]]
    cs = (read <$>) . words <$> init s
    syms = head <$> words (last s)


parse2 :: [String] -> Int
parse2 s = sum $ parsePara <$> paras
  where
    ts = transpose s
    paras = splitOn "     " ts


parsePara :: [String] -> Int
parsePara ls
  | op=='+' = sum $ n1 : ns
  | otherwise = product $ n1 : ns
  where
    (n1, op) = (\s -> (read $ init s, last s))  $ head ls
    ns = read <$> tail ls
    

day6 :: IO ()
day6 = do
  rs <- getF lines 6

  putStrLn $ "Day6: part1: " ++ show (parse1 rs)
  putStrLn $ "Day6: part2: " ++ show (parse2 rs)

  return ()

