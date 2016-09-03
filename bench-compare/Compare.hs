module Main (main) where

import System.Directory (listDirectory)

main :: IO ()
main = do
  files <- listDirectory "data"
  return ()
