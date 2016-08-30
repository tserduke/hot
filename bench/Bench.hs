module Main (main) where

import Criterion.Main
import Criterion.Types

import qualified Data.Hot.GenericBench as Generic


main :: IO ()
main = defaultMainWith config
  [ Generic.benchmarks
  ]

config :: Config
config = defaultConfig {
  timeLimit = 0.5
}
