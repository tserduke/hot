module Data.MonoTupleTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.MonoTuple


tests :: TestTree
tests = testGroup "MonoTuple"
  [ testGroup "merge"
    [ testCase "1" $ merge (T3 1 2 3) (T2 4 5) @?= (T5 1 2 3 4 5)
    ]
  ]
