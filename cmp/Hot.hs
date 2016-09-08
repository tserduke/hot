module Main (main) where

import Criterion.Main

import Data.Hot


main :: IO ()
main = defaultMain $
  [ bgroup "sop"
    [ bench "10 10" $ whnf (sumOrderedProducts (Hot10 4 5 8 6 1 3 7 9 3 10)) (Hot10 7 5 2 1 4 10 2 3 6 4 :: Hot 10 Int)
    ]
  ]


sumOrderedProducts :: (HotClass n) => Hot n Int -> Hot n Int -> Int
sumOrderedProducts xs ys = sum $ (*) <$> sort xs <*> sort ys
