module Foldable (foldableModule) where

import Common
import Data.List (intercalate)


foldableModule :: Function -> Int -> Text
foldableModule inline l = runLines $ do
    "module Data.Hot.Instances.Foldable where"
    ""
    "import Data.Hot.Instances.Base"
    "\n"
    forN l $ \n -> do
        ["instance Foldable (Hot" +++ show n ++ ") where"]
        indent $ do
            pragmaFunc "INLINE" "length" $ "_ =" +++ show n
            inline "foldr" $ "f z" +++ hotMatching n +++ "= f x" ++
                intercalate " (f x" (map show [1 .. n]) +++ "z" ++ replicate (n - 1) ')'
        ""
