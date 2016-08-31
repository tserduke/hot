module Main (main) where

import Test.Tasty

import qualified Data.Hot.GenericTest as Generic
import qualified Data.Hot.SortTest as Sort


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Generic.tests
  , Sort.tests
  ]
