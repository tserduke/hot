module Data.Hot.SortBench (benchmarks) where

import Criterion

import qualified Data.List as L
import Data.Hot
import Data.Hot.Sort


benchmarks :: Benchmark
benchmarks = bgroup "Sort"
  [ bgroup "sort"
    [ bgroup "10"
      [ bench "impl" $ whnf sort (Hot10 2 9 5 7 2 10 7 4 3 1 :: Hot10 Int)
      , bench "list" $ nf L.sort ([2, 9, 5, 7, 2, 10, 7, 4, 3, 1] :: [Int])
      ]
    ]
  ]
