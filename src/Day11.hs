module Day11(day11) where

import Utils
import Data.Map qualified as M
import Data.Map (Map)
import Data.MemoTrie


parse :: [String] -> Map String [String]
parse ss = M.fromList $ parseLine <$> ss
  where
    parseLine s = (init $ ws!!0, tail ws)
      where
        ws = words s
    

part1 :: Map String [String] -> Int
part1 mp = go "you"
  where
    go :: String -> Int
    go = memo $ \pos -> 
      case pos of
        "out" -> 1
        _ -> sum $ go <$> (mp M.! pos)


part2 :: Map String [String] -> Int
part2 mp = go ("svr", False, False)
  where
    go :: (String, Bool, Bool) -> Int
    go = memo $ \(pos, seenFFT, seenDAC) -> 
      case pos of
        "out" -> if seenFFT&&seenDAC then 1 else 0
        "fft" -> sum $ (\p -> go (p, True, seenDAC)) <$> (mp M.! pos)
        "dac" -> sum $ (\p -> go (p, seenFFT, True)) <$> (mp M.! pos)
        _ -> sum $ (\p -> go (p, seenFFT, seenDAC)) <$> (mp M.! pos)


day11 :: IO ()
day11 = do
  ss <- getLines 11
  let devices = parse ss

  putStrLn $ "Day11: part1: " ++ show (part1 devices)
  putStrLn $ "Day11: part2: " ++ show (part2 devices)

  return ()

