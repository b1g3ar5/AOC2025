{- HLINT ignore "Use head" -}

module Day8(day8) where


import Utils
import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as M


type Edge = (Coord3, Coord3)


-- Identify a clique by one of it's members
-- Boss member points to itself
type Graph = Map Coord3 Coord3 


-- Set all coords to point to the clique boss
normalise :: Graph -> Graph
normalise g =  foldl (\h k -> M.insert k (findBoss k h) h) g $ M.keys g


findBoss :: Coord3 -> Graph -> Coord3
findBoss c g
  | c==p = p
  | otherwise = findBoss p g
  where
    p = g M.! c


getCliques :: Graph -> [[Coord3]]
getCliques g = (fst <$>) <$> groupOn snd (sortOn snd $ M.assocs (normalise g))


-- All edges sorted by length
makeEdges ::[Coord3] -> [Edge]
makeEdges ps = snd <$> edges
  where
    edges = sort $ concatMap (\(a:as) -> (\b -> (euclidian3 a b, (a, b))) <$> as) $ init (tails ps)


-- Returns part1 stat (product of 3 largest clique sizes) if it runs out of edges
-- Returns part2 stat (product of last x coords) if it goes down to one clique
makeCliques' :: (Graph, Int) -> [Edge] -> Int
-- Part1
makeCliques' (g,_) [] = product $ take 3 $ sortBy (comparing Down) (length <$> getCliques g)
makeCliques' (g, n) ((a@(xa,_,_),b@(xb,_,_)): fs)
  | newn==1 = xa*xb -- Part2
  | ca==cb = makeCliques' (g, n) fs -- already in the same clique
  | otherwise = makeCliques' (M.insert cb ca g, newn) fs
  where
    newn = n - if ca==cb then 0 else 1
    ca = findBoss a g
    cb = findBoss b g


day8 :: IO ()
day8 = do
  rs <- getF lines 8
  --let rs = test
  let n = 1000
  let coords :: [Coord3]
      coords = (\ws -> (read $ ws!!0, read $ ws!!1, read $ ws!!2)) . splitOn ',' <$> rs
      edges = makeEdges coords
      gstart :: (Graph, Int)
      gstart = (foldl (\m c -> M.insert c c m) M.empty coords, length coords)

  putStrLn $ "Day8: part1: " ++ show (makeCliques' gstart $ take n edges)
  putStrLn $ "Day8: part2: " ++ show (makeCliques' gstart edges)

  return ()


test = ["162,817,812"
  , "57,618,57"
  , "906,360,560"
  , "592,479,940"
  , "352,342,300"
  , "466,668,158"
  , "542,29,236"
  , "431,825,988"
  , "739,650,466"
  , "52,470,668"
  , "216,146,977"
  , "819,987,18"
  , "117,168,530"
  , "805,96,715"
  , "346,949,466"
  , "970,615,88"
  , "941,993,340"
  , "862,61,35"
  , "984,92,344"
  , "425,690,689"]
