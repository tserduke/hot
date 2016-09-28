module Main (main) where

import Common
import Base
import Foldable


main :: IO ()
main = do
    let n = 10
    writeFile "../instances/Data/Hot/Instances/Base.hs" $ runLines $ do
        baseModule
        instanceBase (pragmaFunc "INLINABLE") n
    writeFile "../instances/Data/Hot/Instances/Foldable.hs" $ runLines $ do
        foldableModule
        instanceFoldable (pragmaFunc "INLINABLE") n
