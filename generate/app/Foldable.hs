module Foldable where

import Common
import Data.List (intercalate)


foldableModule :: Lines
foldableModule = do
    "module Data.Hot.Instances.Foldable where"
    ""
    "import Data.Hot.Instances.Base"
    "\n"


instanceFoldable :: Function -> Int -> Lines
instanceFoldable inline x = forN x $ \n -> do
    ["instance Foldable (Hot" +++ show n ++ ") where"]
    indent $ do
        pragmaFunc "INLINE" "length" $ "_ =" +++ show n
        inline "foldr" $ "f z" +++ hotMatching n +++ "= f x" ++
            intercalate " (f x" (map show [1 .. n]) +++ "z" ++ replicate (n - 1) ')'
    ""
