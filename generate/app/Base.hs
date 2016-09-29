module Base (baseModule) where

import Common


baseModule :: Function -> Int -> Text
baseModule inline l = runLines $ do
    "{-# LANGUAGE TypeFamilies #-}"
    ""
    "module Data.Hot.Instances.Base where"
    ""
    "import Data.Hot.Base"
    "import Data.Hot.Internal (hotError)"
    "\n"
    forN l $ \n -> do
        ["instance HotClass" +++ show n +++ "where"]
        indent $ do
            dataHot n
            inline "unfold" $ "f z =" +++
                concat (replicate n "f (") ++ "z Hot" ++ show n ++ replicate n ')'
            inline "elementAt" $ hotMatching n +++ "= \\case"
            indent $ do
                forN n elementAtCase
                hotError "elementAt" n
            inline "mapAt" $ "f" +++ hotMatching n +++ "= \\case"
            indent $ do
                forN n (mapAtCase n)
                hotError "mapAt" n
        ""


dataHot, elementAtCase :: Int -> Lines
dataHot n = do
    ["data Hot" +++ show n +++ "a =" +++ hotConstr n (const "!a")]
    indent "deriving (Eq, Ord, Read, Show)"

elementAtCase i = [show (i - 1) +++ "-> x" ++ show i]

mapAtCase :: Int -> Int -> Lines
mapAtCase n i = [show (i - 1) +++ "->" +++ hotConstr n f] where
    f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j

hotError :: Text -> Int -> Lines
hotError func n = ["x -> hotError" +++ show n +++ "\"" ++ func ++ "\" x"]
