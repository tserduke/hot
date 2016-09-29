module Main (main) where

import Common
import Base
import Foldable


main :: IO ()
main = do
    let n = 10
    writeFile "instances/Data/Hot/Instances/Base.hs" $
        baseModule (pragmaFunc "INLINABLE") n
    writeFile "instances/Data/Hot/Instances/Foldable.hs" $
        foldableModule (pragmaFunc "INLINABLE") n
