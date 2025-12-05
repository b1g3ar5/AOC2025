module Day4(day4) where

import Utils
import Data.Set (Set)
import qualified Data.Set as S


moveable :: Set Coord -> Set Coord
moveable cs = S.filter ((<4). length . filter (`S.member` cs) . neighbours8) cs


remove :: Set Coord -> Set Coord
remove cs
  | S.size move == 0 = cs
  | otherwise = remove $ cs S.\\ move
    where
      move = moveable cs


day4 :: IO ()
day4 = do
  ss <- getLines  4
  let g :: [(Coord, Bool)]
      g = parseGridWith (=='@') ss
      gg = S.fromList $ fst <$> filter snd g

  putStrLn $ "Day4: part1: " ++ show (S.size $ moveable gg)
  putStrLn $ "Day4: part2: " ++ show (S.size gg - S.size (remove gg))

  return ()

