module Main (main) where

import Criterion.Main
import Criterion.Types

import qualified Data.Hot.GenericBench as Generic
import qualified Data.Hot.SortBench as Sort


main :: IO ()
main = defaultMainWith config
  [ Generic.benchmarks
  , Sort.benchmarks
  ]


config :: Config
config = defaultConfig {
  timeLimit = 0.5
}
