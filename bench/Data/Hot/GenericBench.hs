module Data.Hot.GenericBench (benchmarks) where

import Criterion

import qualified Data.List.Extra as L
import Data.Hot
import Data.Hot.Generic


benchmarks :: Benchmark
benchmarks = bgroup "Hot"
  [ bgroup "merge"
    [ bgroup "5"
      [ bench "merge" $ whnf (merge (Hot2 1 3)) (Hot3 2 4 6 :: Hot3 Int)
      , bench "list" $ nf (L.merge [1, 3]) ([2, 4, 6] :: [Int])
      ]
    , bgroup "10"
      [ bench "merge" $ whnf (merge (Hot4 1 3 5 7)) (Hot6 2 4 6 8 10 12 :: Hot6 Int)
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
