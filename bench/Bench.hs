module Main where

import Criterion.Main
import Criterion.Types

import Data.HotBench


main :: IO ()
main = defaultMainWith config
  [ benchHot
  ]

config :: Config
config = defaultConfig {
  timeLimit = 0.5
}
