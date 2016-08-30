module Data.MonoTupleBench (benchTuple) where

import Criterion

import qualified Data.List.Extra as E
import Data.MonoTuple


benchTuple :: Benchmark
benchTuple = bgroup "MonoTuple"
  [ bgroup "elementAt"
    [ bench "impl" $ whnf (sum . map (elementAt t10)) [0 .. 9]
    , bench "matching" $ whnf (sum . map (elementAt10 t10)) [0 .. 9]
    ]
  , bgroup "merge"
    [ bgroup "5"
      [ bench "merge" $ whnf (merge (T2 (1 :: Int) 3)) (T3 2 4 6)
      , bench "list" $ nf (E.merge [(1 :: Int), 3]) [2, 4, 6]
      ]
    , bgroup "10"
      [ bench "merge" $ whnf (merge (T4 (1 :: Int) 3 5 7)) (T6 2 4 6 8 10 12)
      , bench "stream" $ whnf (last . E.merge (enumFromThenTo (1 :: Int) 3 7)) (enumFromThenTo 2 4 12)
      , bench "list" $ nf (E.merge [(1 :: Int), 3, 5, 7]) [2, 4, 6, 8, 10, 12]
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
