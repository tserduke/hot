module Main (main) where

import Criterion.Main

import Data.Hot


main :: IO ()
main = defaultMain $
  [ bgroup "sub"
    [ bench "prefix" $ whnf (prefix :: Hot 10 Int -> Hot 5 Int) (Hot10 1 2 3 4 5 6 7 8 9 10)
    , bench "suffix" $ whnf (suffix :: Hot 10 Int -> Hot 5 Int) (Hot10 1 2 3 4 5 6 7 8 9 10)
    ]
  , bgroup "merge"
    [ bench "2 3" $ whnf (merge (Hot2 1 3)) (Hot3 2 4 6 :: Hot 3 Int)
    , bench "4 6" $ whnf (merge (Hot4 1 3 5 7)) (Hot6 2 4 6 8 10 12 :: Hot 6 Int)
    , bench "9 1" $ whnf (merge (Hot9 1 2 3 4 5 6 7 8 9)) (Hot1 2 :: Hot 1 Int)
    ]
  , bgroup "sort"
    [ bench "10" $ whnf sort (Hot10 2 9 5 7 2 10 7 4 3 1 :: Hot 10 Int)
    ]
  ]
