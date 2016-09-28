module Main (main) where

import Data.DoList (DoList, item, toList)
import Data.List (intercalate)


type Text = String

main :: IO ()
main = do
  let n = 10
  writeFile "inline/Data/Hot/Base.hs" $ baseModule (pragmaFunc "INLINE" 1) n
  writeFile "inlinable/Data/Hot/Base.hs" $ baseModule (pragmaFunc "INLINABLE" 1) n


baseModule :: (Text -> Text -> Line) -> Int -> Text
baseModule inline n = runLines $ do
  "{-# LANGUAGE KindSignatures, Rank2Types, TypeFamilies #-}"
  ""
  "module Data.Hot.Base where"
  ""
  "import Data.Hot.Internal (hotError)"
  "import GHC.TypeLits (Nat)"
  "\n"
  "class (Foldable (Hot n)) => HotClass (n :: Nat) where"
  tab 1 "data Hot n :: * -> *"
  tab 1 "unfold :: (forall r. c (a -> r) -> c r) -> (forall r. r -> c r) -> c (Hot n a)"
  tab 1 "elementAt :: Hot n a -> Int -> a"
  tab 1 "mapAt :: (a -> a) -> Hot n a -> Int -> Hot n a"
  "\n"
  forN n (instanceHot inline)
  ""
  forN n (instanceFoldable inline)


pragmaFunc :: Text -> Int -> Text -> Text -> Line
pragmaFunc pragma ind func body = do
  tab ind $ "{-#" +++ pragma +++ func +++ "#-}"
  tab ind $ func +++ body

instanceHot, instanceFoldable  :: (Text -> Text -> Line) -> Int -> Line
instanceHot inline n = do
  line $ "instance HotClass" +++ show n +++ "where"
  dataHot n
  inline "unfold" $ "f z =" +++ concat (replicate n "f (") ++ "z Hot" ++ show n ++ replicate n ')'
  inline "elementAt" $ hotMatching n +++ "= \\case"
  forN n elementAtCase
  tab 2 $ "n -> hotError" +++ show n +++ "\"elementAt\" n"
  inline "mapAt" $ "f" +++ hotMatching n +++ "= \\case"
  forN n (mapAtCase n)
  tab 2 $ "n -> hotError" +++ show n +++ "\"mapAt\" n"
  ""

instanceFoldable inline n = do
  line $ "instance Foldable (Hot" +++ show n ++ ") where"
  pragmaFunc "INLINE" 1 "length" $ "_ =" +++ show n
  inline "foldr" $ "f z" +++ hotMatching n +++ "= f x" ++
    intercalate " (f x" (map show [1 .. n]) +++ "z" ++ replicate (n - 1) ')'
  ""


dataHot, elementAtCase :: Int -> Line
dataHot n = do
  tab 1 $ "data Hot" +++ show n +++ "a =" +++ hotConstr n (const "!a")
  tab 2 "deriving (Eq, Ord, Read, Show)"

elementAtCase i = tab 2 $ show (i - 1) +++ "-> x" ++ show i

mapAtCase :: Int -> Int -> Line
mapAtCase n i = tab 2 $ show (i - 1) +++ "->" +++ hotConstr n f where
  f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j

hotMatching :: Int -> Text
hotMatching n = "(" ++ hotConstr n (("x" ++) . show) ++ ")"

hotConstr :: Int -> (Int -> Text) -> Text
hotConstr n f = "Hot" ++ show n +++ unwords (map f [1 .. n])


forN :: (Monad m) => Int -> (Int -> m a) -> m a
forN n f = foldr1 (>>) $ map f [1 .. n]

(+++) :: Text -> Text -> Text
x +++ y = x ++ " " ++ y

tab :: Int -> Text -> Line
tab n x = line $ concat (replicate n "    ") ++ x


type Line = DoList Text

line :: Text -> Line
line = item

runLines :: Line -> Text
runLines = unlines . toList
