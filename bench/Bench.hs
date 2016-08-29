module Main where

import Criterion.Main
import Criterion.Types

import Data.MonoTuple.GenericBench


main :: IO ()
main = defaultMainWith config [
  benchGeneric
  ]

config :: Config
config = defaultConfig {
  timeLimit = 0.5
}
