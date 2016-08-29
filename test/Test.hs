module Main where

import Test.Tasty

import qualified Data.MonoTupleTest as MonoTuple


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ MonoTuple.tests
  ]
