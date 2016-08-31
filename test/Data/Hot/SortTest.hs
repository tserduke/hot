module Data.Hot.SortTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Hot
import Data.Hot.Sort


tests :: TestTree
tests = testGroup "Sort"
  [ testCase "1" $ sort (Hot1 True) @?= (Hot1 True)
  , testCase "2" $ sort (Hot2 True False) @?= (Hot2 False True)
  , testCase "3" $ sort (Hot3 5 1 3) @?= (Hot3 1 3 5 :: Hot3 Int)
  , testCase "7" $ sort (Hot7 4 5 3 7 2 6 1) @?= (Hot7 1 2 3 4 5 6 7 :: Hot7 Int)
  , testCase "10" $ sort (Hot10 1 9 6 2 5 3 7 4 10 8) @?= (Hot10 1 2 3 4 5 6 7 8 9 10 :: Hot10 Int)
  ]
