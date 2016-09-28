module Main (main) where

import Common (pragmaFunc)
import Base (baseModule)


main :: IO ()
main = do
    let n = 10
    writeFile "../instances/Data/Hot/Instances.hs" $
        baseModule (pragmaFunc "INLINABLE") n
