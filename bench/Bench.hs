module Main where

import Criterion.Main
import Criterion.Types

import Data.HotBench


main :: IO ()
main = defaultMainWith config
  [ benchTuple
  ]

config :: Config
config = defaultConfig {
  timeLimit = 0.5
}
