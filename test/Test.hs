module Main (main) where

import Test.Tasty

import qualified Data.Hot.GenericTest as Generic


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Generic.tests
  ]
