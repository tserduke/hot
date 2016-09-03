module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Hot


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "prefix" $
    [ testCase "5 3" $ prefix (Hot5 1 2 3 4 5) @?= (Hot3 1 2 3 :: Hot3 Int)
    , testCase "2 1" $ prefix (Hot2 1 2) @?= (Hot1 1 :: Hot1 Int)
    , testCase "3 3" $ prefix (Hot3 1 2 3) @?= (Hot3 1 2 3 :: Hot3 Int)
    , testCase "1 1" $ prefix (Hot1 1) @?= (Hot1 1 :: Hot1 Int)
    ]
  , testGroup "suffix" $
    [ testCase "5 3" $ suffix (Hot5 1 2 3 4 5) @?= (Hot3 3 4 5 :: Hot3 Int)
    , testCase "2 1" $ suffix (Hot2 1 2) @?= (Hot1 2 :: Hot1 Int)
    , testCase "3 3" $ suffix (Hot3 1 2 3) @?= (Hot3 1 2 3 :: Hot3 Int)
    , testCase "1 1" $ suffix (Hot1 1) @?= (Hot1 1 :: Hot1 Int)
    ]
  , testGroup "merge"
    [ testCase "3 2" $ merge (Hot3 1 2 3) (Hot2 4 5) @?= (Hot5 1 2 3 4 5 :: Hot5 Int)
    , testCase "1 2" $ merge (Hot1 2) (Hot2 1 3) @?= (Hot3 1 2 3 :: Hot3 Int)
    , testCase "4 5" $ merge (Hot4 4 4 4 4) (Hot5 5 5 5 5 5) @?= (Hot9 4 4 4 4 5 5 5 5 5 :: Hot9 Int)
    , testCase "2 2" $ merge (Hot2 4 5) (Hot2 1 2) @?= (Hot4 1 2 4 5 :: Hot4 Int)
    ]
  , testGroup "sort"
    [ testCase "1" $ sort (Hot1 True) @?= (Hot1 True)
    , testCase "2" $ sort (Hot2 True False) @?= (Hot2 False True)
    , testCase "3" $ sort (Hot3 5 1 3) @?= (Hot3 1 3 5 :: Hot3 Int)
    , testCase "7" $ sort (Hot7 4 5 3 7 2 6 1) @?= (Hot7 1 2 3 4 5 6 7 :: Hot7 Int)
    , testCase "10" $ sort (Hot10 1 9 6 2 5 3 7 4 10 8) @?= (Hot10 1 2 3 4 5 6 7 8 9 10 :: Hot10 Int)
    ]
  ]
