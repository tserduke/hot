module Main (main) where

import Criterion.Main

import Data.Hot.Pidgin

main :: IO ()
main = defaultMain $
  [ bgroup "sub"
    [ bench "prefix" $ whnf (prefix5of10 :: PHot10 Int -> PHot5 Int) t10
    , bench "suffix" $ whnf (suffix5of10 :: PHot10 Int -> PHot5 Int) t10
    ]
  , bgroup "merge"
    [ bench "2 3" $ whnf (merge2and3 (PHot2 1 3)) (PHot3 2 4 6 :: PHot3 Int)
    ]
  , bgroup "fmap"
    [ bench "10 1" $ whnf (fmap (* 2)) t10
    , bench "10 2" $ whnf (fmap (`mod` 5) . fmap (* 2)) t10
    , bench "10 2." $ whnf (fmap ((`mod` 5) . (* 2))) t10
    ]
  ]

t10 :: PHot10 Int
t10 = PHot10 1 2 3 4 5 6 7 8 9 10
