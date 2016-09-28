module Main (main) where

import Data.DoList (DoList, fromList, item, toList)
import Data.List (intercalate)


type Text = String

main :: IO ()
main = do
    let n = 10
    writeFile "../instances/Data/Hot/Instances.hs" $ baseModule (pragmaFunc "INLINABLE") n


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


pragmaFunc :: Text -> Text -> Text -> Line
pragmaFunc pragma func body = do
    item $ "{-#" +++ pragma +++ func +++ "#-}"
    item $ func +++ body

instanceHot, instanceFoldable :: (Text -> Text -> Line) -> Int -> Line
instanceHot inline n = do
    line $ "instance HotClass" +++ show n +++ "where"
    indent $ do
        dataHot n
        inline "unfold" $ "f z =" +++ concat (replicate n "f (") ++ "z Hot" ++ show n ++ replicate n ')'
        inline "elementAt" $ hotMatching n +++ "= \\case"
        indent $ do
            forN n elementAtCase
            item $ "n -> hotError" +++ show n +++ "\"elementAt\" n"
        inline "mapAt" $ "f" +++ hotMatching n +++ "= \\case"
        indent $ do
            forN n (mapAtCase n)
            item $ "n -> hotError" +++ show n +++ "\"mapAt\" n"
    ""

instanceFoldable inline n = do
    line $ "instance Foldable (Hot" +++ show n ++ ") where"
    indent $ do
        pragmaFunc "INLINE" "length" $ "_ =" +++ show n
        inline "foldr" $ "f z" +++ hotMatching n +++ "= f x" ++
            intercalate " (f x" (map show [1 .. n]) +++ "z" ++ replicate (n - 1) ')'
    ""


dataHot, elementAtCase :: Int -> Line
dataHot n = do
    item $ "data Hot" +++ show n +++ "a =" +++ hotConstr n (const "!a")
    indent "deriving (Eq, Ord, Read, Show)"

elementAtCase i = item $ show (i - 1) +++ "-> x" ++ show i

mapAtCase :: Int -> Int -> Line
mapAtCase n i = item $ show (i - 1) +++ "->" +++ hotConstr n f where
    f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j

hotMatching :: Int -> Text
hotMatching n = "(" ++ hotConstr n (("x" ++) . show) ++ ")"

hotConstr :: Int -> (Int -> Text) -> Text
hotConstr n f = "Hot" ++ show n +++ unwords (map f [1 .. n])


forN :: (Monad m) => Int -> (Int -> m a) -> m a
forN n f = foldr1 (>>) $ map f [1 .. n]

(+++) :: Text -> Text -> Text
x +++ y = x ++ " " ++ y

indent :: Line -> Line
indent = fromList . map ("    " ++) . toList


type Line = DoList Text

line :: Text -> Line
line = item

runLines :: Line -> Text
runLines = unlines . toList
