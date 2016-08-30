module Main where

import Test.Tasty

import qualified Data.HotTest as Tuple


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Tuple.tests
  ]
