module Day10(day10) where

import Utils
import Data.Bits
import Control.Monad
import Control.DeepSeq
import Control.Concurrent
import Data.SBV
import Data.SBV.Internals (CV)
import Control.Arrow
import Data.Map qualified as M
import Numeric.LinearAlgebra hiding ((<>))
import Numeric.LinearAlgebra qualified as LA

type Machine1 = (Int, [Int], Int )
type Machine2 = ([[Int]], [Int] )

makeSystem :: Machine2 -> (Matrix Double, Vector Double)
makeSystem (ixss, bs) = (fromLists $ transpose m, vector $ fromIntegral <$> bs)
  where
    mix = maximum $ concat ixss
    len = length ixss
    m :: [[Double]]
    m = (\ixs -> (\r -> if r `elem` ixs then 1 else 0) <$> [0..mix]) <$> ixss

makeSystem' :: Machine2 -> (Matrix Double, Matrix Double)
makeSystem' (ixss, bs) = (fromLists $ transpose m, matrix 1 $ fromIntegral <$> bs)
  where
    mix = maximum $ concat ixss
    len = length ixss
    m :: [[Double]]
    m = (\ixs -> (\r -> if r `elem` ixs then 1 else 0) <$> [0..mix]) <$> ixss




parse1 :: String -> Machine1
parse1 s = (convert $ init (tail $ head ps), parse' <$> init (tail ps) , parse' $ ps!!(len-1))
  where
    ps = words s
    len = length ps
    convert :: String -> Int
    convert s = foldl setBit zeroBits ixs
      where
        ixs = snd <$> filter ((=='#').fst) (zip s [0..])

    parse' :: String -> Int
    parse' s = foldl setBit zeroBits ixs
      where
        ixs = read <$> splitOn ',' (init $ tail s)

parse2 :: String -> Machine2
parse2 s = (buttons, fromIntegral <$> joltage)
  where
    buttons = parse' <$> init (tail ps)
    ps = words s
    len = length ps
    joltage :: [Int]
    joltage = read <$> splitOn ',' (init $ tail $ ps!!(len-1))
    parse' :: String -> [Int]
    parse' s = read <$> splitOn ',' (init $ tail s)


solve1 :: [Int] -> Int -> [Int]
solve1 buttons target = do
  c <- [0..4095::Int]
  guard $ foldl (\acc (b, ix) -> acc `xor` (if c `testBit` ix then b else 0)) 0 (zip buttons [0..]) == target
  return $ popCount c


fewestPresses :: Machine2 -> IO Integer
fewestPresses (buttons, joltages) = getTotalPresses <$> optimize Lexicographic (do
  -- create SMT variables for each button
  vs <- mapM mkVar [0 .. length buttons - 1]
  -- assert constraints for each state/joltage
  zipWithM_ (addConstraint vs) [0..] joltages
  -- minimize the sum of all button variables (total presses)
  minimize "total_presses" (sum $ map snd vs))
  --minimize "total_presses" (foldl (\acc v -> acc * 1000 + v) 1 (snd <$> vs)))
  where
    addConstraint :: [(Int, SBV Integer)] -> Int -> Int -> SymbolicT IO ()
    addConstraint presses idx a =
      constrain (literal (toInteger a) .== sum [ v | (btnIdx, v) <- presses, idx `elem` (buttons !! btnIdx) ])
    -- Positive integers only
    mkVar :: Int -> SymbolicT IO (Int, SBV Integer)
    mkVar idx = do
      v <- symbolic (show idx)
      constrain (v .>= 0)
      return (idx, v)
    getTotalPresses :: OptimizeResult -> Integer
    getTotalPresses (LexicographicResult model)
      | Just n <- getModelValue "total_presses" model = n
    getTotalPresses _ = error "Optimization failed"



parMapM :: NFData b => (a -> IO b) -> [a] -> IO [b]
parMapM f xs = do
  num <- getNumCapabilities
  let c = max 1 (length xs `div` num)
  vs <- mapM go (chunksOf c xs)
  concat <$> mapM readMVar vs
  where
    go x = do
      v <- newEmptyMVar
      _ <- forkIO $ do
        res <- mapM f x
        putMVar v $! force res
      return v

epsilon = 0.001

convert x
  | (x - fromIntegral (round x)) <= epsilon = round x
  | otherwise = -1


day10 :: IO ()
day10 = do
  ss <- getLines 10
  --let ss = test
  let machines1 = parse1 <$> ss
      machines2 = parse2 <$> ss

  putStrLn $ "Day10: part1: " ++ show (sum $ (\(t,b,_) -> minimum $ solve1 b t) <$> machines1 )
  res2 <- parMapM fewestPresses machines2

  let (m, v) = makeSystem (machines2!!3)
  let (_, v') = makeSystem' (machines2!!3)

  putStrLn $ "Day10: part2: " ++ show (sum res2)
  --putStrLn $ "Day10: part2: " ++ show (res2!!1)
  --putStrLn $ "Day10: part2: " ++ show (m, v)
  --putStrLn $ "Day10: part2:<\\> " ++ show (m <\> v)
  --putStrLn $ "Day10: part2:lu " ++ show ( lu m)
  --putStrLn $ "Day10: part2:pinv " ++ show ( pinv m LA.<> v')
  --putStrLn $ "Day10: part2:linearSolveLS " ++ show ( uncurry linearSolveLS . makeSystem' $ machines2!!0)

  return ()

test = ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
  , "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
  , "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
  ]

