module Data.HotTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Hot


tests :: TestTree
tests = testGroup "MonoTuple"
  [ testGroup "merge"
    [ testCase "1" $ merge (Hot3 (1 :: Int) 2 3) (Hot2 4 5) @?= (Hot5 1 2 3 4 5)
    , testCase "2" $ merge (Hot1 (2 :: Int)) (Hot2 1 3) @?= (Hot3 1 2 3)
    ]
  ]
