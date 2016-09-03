module Main (main) where

import Criterion.Main
import Criterion.Types

import Data.Hot
import qualified Data.List.Extra as L


main :: IO ()
main = defaultMainWith config $
  [ bgroup "sub"
    [ bench "prefix" $ whnf (prefix :: Hot 10 Int -> Hot 5 Int) (Hot10 1 2 3 4 5 6 7 8 9 10)
    , bench "suffix" $ whnf (suffix :: Hot 10 Int -> Hot 5 Int) (Hot10 1 2 3 4 5 6 7 8 9 10)
    ]
  , bgroup "merge"
    [ bgroup "2 3"
      [ bench "impl" $ whnf (merge (Hot2 1 3)) (Hot3 2 4 6 :: Hot 3 Int)
      , bench "list" $ nf (L.merge [1, 3]) ([2, 4, 6] :: [Int])
      ]
    , bgroup "4 6"
      [ bench "impl" $ whnf (merge (Hot4 1 3 5 7)) (Hot6 2 4 6 8 10 12 :: Hot 6 Int)
      , bench "stream" $ whnf (last . L.merge (enumFromThenTo 1 3 7)) (enumFromThenTo 2 4 12 :: [Int])
      , bench "list" $ nf (L.merge [1, 3, 5, 7]) ([2, 4, 6, 8, 10, 12] :: [Int])
      ]
    , bgroup "9 1"
      [ bench "impl" $ whnf (merge (Hot9 1 2 3 4 5 6 7 8 9)) (Hot1 2 :: Hot 1 Int)
      , bench "stream" $ whnf (last . L.merge (enumFromTo 1 9)) ([1] :: [Int])
      ]
    ]
  , bgroup "sort"
    [ bgroup "10"
      [ bench "impl" $ whnf sort (Hot10 2 9 5 7 2 10 7 4 3 1 :: Hot 10 Int)
      , bench "list" $ nf L.sort ([2, 9, 5, 7, 2, 10, 7, 4, 3, 1] :: [Int])
      ]
    ]
  ]

config :: Config
config = defaultConfig {
  timeLimit = 0.5
}
