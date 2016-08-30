module Main (main) where

import Test.Tasty

import qualified Data.HotTest as Hot


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Hot.tests
  ]
