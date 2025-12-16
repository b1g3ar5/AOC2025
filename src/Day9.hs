{- HLINT ignore "Use head" -}

module Day9(day9) where


import Utils (Coord, getF, splitOn)


type Line = (Coord, Coord)


area :: Coord -> Coord -> Int
area (x1,y1) (x2,y2) = (1 + abs (x2-x1))*(1 + abs (y2-y1))


-- Checks that there are no crosses of the border
noOverlaps :: Line -> [Line] -> Bool
noOverlaps ((p1, q1), (p2, q2)) = all noOverlap
  where
    noOverlap :: Line -> Bool
    noOverlap ((x1, y1), (x2, y2)) =
         (max x1 x2 <= min p1 p2) -- smallest x is bigger than the biggest border
      || (min x1 x2 >= max p1 p2) -- biggest x is smaller than the smallest border
      || (max y1 y2 <= min q1 q2) -- smallest y is bigger than the biggest border
      || (min y1 y2 >= max q1 q2) -- biggest y is smaller than the smallest border


day9 :: IO ()
day9 = do
  rs <- getF lines 9
  --let rs = test
  let coords :: [Coord]
      coords = (\v -> (v!!0, v!!1)) . (read <$>) . splitOn ',' <$> rs
      border :: [Line]
      border = zip coords $ tail coords ++ [head coords]
      rects1 = [area c1 c2 | c1<-coords, c2<-coords, c1/=c2]
      rects2 = [area c1 c2 | c1<-coords, c2<-coords, c1/=c2, noOverlaps (c1, c2) border]


  putStrLn $ "Day9: part1: " ++ show (maximum rects1)
  putStrLn $ "Day9: part2: " ++ show (maximum rects2)

  return ()


test = ["7,1"
  , "11,1"
  , "11,7"
  , "9,7"
  , "9,5"
  , "2,5"
  , "2,3"
  , "7,3"]

