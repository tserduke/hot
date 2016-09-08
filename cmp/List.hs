module Main (main) where

import Criterion.Main

import Data.List


main :: IO ()
main = defaultMain $
  [ bgroup "sop"
    [ bench "10 10" $ whnf (sumOrderedProducts [4, 5, 8, 6, 1, 3, 7, 9, 3, 10]) ([7, 5, 2, 1, 4, 10, 2, 3, 6, 4] :: [Int])
    ]
  ]


sumOrderedProducts :: [Int] -> [Int] -> Int
sumOrderedProducts xs ys = sum $ zipWith (*) (sort xs) (sort ys)
