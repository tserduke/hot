module Data.MonoTupleBench (benchTuple) where

import Criterion

import Data.MonoTuple


benchTuple :: Benchmark
benchTuple = bgroup "Generic"
  [ bgroup "elementAt"
    [ bench "impl" $ whnf (sum . map (elementAt t10)) [0 .. 9]
    , bench "matching" $ whnf (sum . map (elementAt10 t10)) [0 .. 9]
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
