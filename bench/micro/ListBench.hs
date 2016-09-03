module Main (main) where

import Criterion.Main

import Data.List.Extra


main :: IO ()
main = defaultMain $
  [ bgroup "merge"
    [ bench "2 3" $ nf (merge [1, 3]) ([2, 4, 6] :: [Int])
    , bench "4 6" $ nf (merge [1, 3]) ([2, 4, 6] :: [Int])
    ]
  , bgroup "sort"
    [ bench "10" $ nf sort ([2, 9, 5, 7, 2, 10, 7, 4, 3, 1] :: [Int])
    ]
  ]
