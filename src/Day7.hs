{- HLINT ignore "Use head" -}

module Day7(day7) where


import Utils (bimap, Coord, getF, parseGridWith)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S


-- I also tried to do this backwards with memoisation - but it was slower in this simple example

data Cell = Start | Splitter | Empty deriving (Eq, Show)
type Grid a = Map Coord a


notInGrid :: Coord -> Grid a -> Bool
notInGrid c g = c `M.notMember` g


parseCell :: Char -> Cell
parseCell '.' = Empty
parseCell 'S' = Start
parseCell '^' = Splitter
parseCell _ = error "Must be [.S^]"


unzip' :: (Ord a, Ord b) => Set (a, b) -> (Set a, Set b)
unzip' = S.fold (\ (l, r) (ls, rs) -> (S.insert l ls, S.insert r rs)) (S.empty, S.empty)


-- splitter coords
getSplitters :: Set Coord -> Grid Cell -> Int
getSplitters cs g
  | S.null positions = 0
  | otherwise = S.size splitters + getSplitters positions g
    where
      (splitters, positions) = bimap S.unions S.unions $ unzip' $ S.map next cs
      next :: Coord -> (Set Coord, Set Coord)
      next (x,y)
        | (x,y+1) `notInGrid` g = (S.empty, S.empty)
        | g M.! (x,y+1) == Splitter = (S.singleton (x,y+1), S.fromList [(x-1, y+1), (x+1, y+1)])
        | otherwise = (S.empty, S.singleton (x, y+1))


routes :: Grid Int -> Grid Cell -> Grid Int
routes cs g
  | M.null ns = cs
  | otherwise = routes ns g
    where
      ns :: Grid Int
      ns = M.unionsWith (+) $ M.elems $ M.mapWithKey next cs
      next :: Coord -> Int -> Grid Int
      next (x,y) n
        | (x,y+1) `notInGrid` g = M.empty
        | g M.! (x,y+1) == Splitter = M.fromList [((x-1, y+1), n), ((x+1, y+1), n)]
        | otherwise = M.fromList [((x, y+1), n)]  


day7 :: IO ()
day7 = do
  rs <- getF lines 7
  --let rs = test
  let g :: Grid Cell
      g = M.fromList $ parseGridWith parseCell rs
      start = fst $ head $ M.toList $ M.filter (==Start) g

  putStrLn $ "Day7: part1: " ++ show (getSplitters (S.singleton start) g)
  putStrLn $ "Day7: part2: " ++ show (sum $ M.elems $ routes (M.singleton start 1) g)

  return ()


-------------------------------- CODE GRAVEYARD ------------------------------------
{-

import Data.MemoTrie


routes' :: Grid Cell -> Coord -> Grid Int
routes' g start = M.mapWithKey (\k _ -> go k) g
  where
    go :: Coord -> Int
    go = memo $ \ (x,y) -> 
      case (x,y) of
        (70, 0) -> 1
        (_, 0) -> 0
        (0, q) -> go (0, q-1)
                  + if g M.! (1,q) == Splitter then go (1,q-1) else 0 
        (140, q) -> go (14, q-1)
                  + if g M.! (139,q) == Splitter then go (139,q-1) else 0
        (p, q) -> if g M.! (p,q) == Splitter then 0 else
                    go (p, q-1)
                    + (if g M.! (p-1,q) == Splitter then go (p-1,q-1) else 0)
                    + (if g M.! (p+1,q) == Splitter then go (p+1,q-1) else 0)


maxx, maxy, startx :: Int
maxx = 140
maxy = 140
startx = 70


test = [".......S......."
  , "..............."
  , ".......^......."
  , "..............."
  , "......^.^......"
  , "..............."
  , ".....^.^.^....."
  , "..............."
  , "....^.^...^...."
  , "..............."
  , "...^.^...^.^..."
  , "..............."
  , "..^...^.....^.."
  , "..............."
  , ".^.^.^.^.^...^."
  , "..............."]

-}