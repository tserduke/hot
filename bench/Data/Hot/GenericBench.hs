module Data.Hot.GenericBench (benchmarks) where

import Criterion

import qualified Data.List.Extra as L
import Data.Hot
import Data.Hot.Generic


benchmarks :: Benchmark
benchmarks = bgroup "Hot"
  [ bgroup "sub"
    [ bench "prefix" $ whnf (prefix :: Hot10 Int -> Hot5 Int) (Hot10 1 2 3 4 5 6 7 8 9 10)
    , bench "suffix" $ whnf (suffix :: Hot10 Int -> Hot5 Int) (Hot10 1 2 3 4 5 6 7 8 9 10)
    ]
  , bgroup "merge"
    [ bgroup "2 3"
      [ bench "impl" $ whnf (merge (Hot2 1 3)) (Hot3 2 4 6 :: Hot3 Int)
      , bench "list" $ nf (L.merge [1, 3]) ([2, 4, 6] :: [Int])
      ]
    , bgroup "4 6"
      [ bench "impl" $ whnf (merge (Hot4 1 3 5 7)) (Hot6 2 4 6 8 10 12 :: Hot6 Int)
      , bench "stream" $ whnf (last . L.merge (enumFromThenTo 1 3 7)) (enumFromThenTo 2 4 12 :: [Int])
      , bench "list" $ nf (L.merge [1, 3, 5, 7]) ([2, 4, 6, 8, 10, 12] :: [Int])
      ]
    , bgroup "9 1"
      [ bench "impl" $ whnf (merge (Hot9 1 2 3 4 5 6 7 8 9)) (Hot1 2 :: Hot1 Int)
      , bench "stream" $ whnf (last . L.merge (enumFromTo 1 9)) ([1] :: [Int])
      ]
    ]
  , bgroup "sort"
    [ bgroup "10"
      [ bench "list" $ nf L.sort ([2, 9, 5, 7, 2, 10, 7, 4, 3, 1] :: [Int])
      ]
    ]
  ]
