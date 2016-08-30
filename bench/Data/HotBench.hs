module Data.HotBench (benchTuple) where

import Criterion

import qualified Data.List.Extra as L
import Data.Hot


benchTuple :: Benchmark
benchTuple = bgroup "MonoTuple"
  [ bgroup "elementAt"
    [ bench "impl" $ whnf (sum . map (elementAt t10)) [0 .. 9]
    , bench "matching" $ whnf (sum . map (elementAt10 t10)) [0 .. 9]
    ]
  , bgroup "merge"
    [ bgroup "5"
      [ bench "merge" $ whnf (merge (T2 1 3)) (T3 2 4 6 :: Tuple3 Int)
      , bench "list" $ nf (L.merge [1, 3]) ([2, 4, 6] :: [Int])
      ]
    , bgroup "10"
      [ bench "merge" $ whnf (merge (T4 1 3 5 7)) (T6 2 4 6 8 10 12 :: Tuple6 Int)
      , bench "stream" $ whnf (last . L.merge (enumFromThenTo 1 3 7)) (enumFromThenTo 2 4 12 :: [Int])
      , bench "list" $ nf (L.merge [1, 3, 5, 7]) ([2, 4, 6, 8, 10, 12] :: [Int])
      ]
    ]
  , bgroup "sort"
    [ bgroup "10"
      [ bench "list" $ nf L.sort ([2, 9, 5, 7, 2, 10, 7, 4, 3, 1] :: [Int])
      ]
    ]
  ]

t10 :: Tuple10 Int
t10 = T10 1 2 3 4 5 6 7 8 9 10


elementAt10 :: Tuple10 Int -> Int -> Int
elementAt10 (T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = \case
  0 -> x1
  1 -> x2
  2 -> x3
  3 -> x4
  4 -> x5
  5 -> x6
  6 -> x7
  7 -> x8
  8 -> x9
  9 -> x10
  n -> error $ show n
