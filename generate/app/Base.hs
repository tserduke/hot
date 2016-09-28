module Base (baseModule) where

import Common
import Foldable


baseModule :: (Text -> Text -> Line) -> Int -> Text
baseModule inline n = runLines $ do
    "{-# LANGUAGE TypeFamilies #-}"
    ""
    "module Data.Hot.Instances where"
    ""
    "import Data.Hot.Base"
    "import Data.Hot.Internal (hotError)"
    "\n"
    forN n (instanceHot inline)
    ""
    forN n (instanceFoldable inline)

instanceHot :: (Text -> Text -> Line) -> Int -> Line
instanceHot inline n = do
    line $ "instance HotClass" +++ show n +++ "where"
    indent $ do
        dataHot n
        inline "unfold" $ "f z =" +++
            concat (replicate n "f (") ++ "z Hot" ++ show n ++ replicate n ')'
        inline "elementAt" $ hotMatching n +++ "= \\case"
        indent $ do
            forN n elementAtCase
            line $ "n -> hotError" +++ show n +++ "\"elementAt\" n"
        inline "mapAt" $ "f" +++ hotMatching n +++ "= \\case"
        indent $ do
            forN n (mapAtCase n)
            line $ "n -> hotError" +++ show n +++ "\"mapAt\" n"
    ""


dataHot, elementAtCase :: Int -> Line
dataHot n = do
    line $ "data Hot" +++ show n +++ "a =" +++ hotConstr n (const "!a")
    indent "deriving (Eq, Ord, Read, Show)"

elementAtCase i = line $ show (i - 1) +++ "-> x" ++ show i

mapAtCase :: Int -> Int -> Line
mapAtCase n i = line $ show (i - 1) +++ "->" +++ hotConstr n f where
    f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j
