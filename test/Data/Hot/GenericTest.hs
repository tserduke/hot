module Data.Hot.GenericTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Hot
import Data.Hot.Generic


tests :: TestTree
tests = testGroup "MonoTuple"
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
    ]
  ]
