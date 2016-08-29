module Main where

import Criterion.Main
import Criterion.Types


main :: IO ()
main = defaultMainWith config [
	]

config :: Config
config = defaultConfig {
	timeLimit = 0.5
}

