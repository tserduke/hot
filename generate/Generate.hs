module Main where

import BasicPrelude


main :: IO ()
main = writeFile "../src/Data/Hot/Base.hs" (baseModule 10)


baseModule :: Int -> Text
baseModule n = runText $ do
  "{-# LANGUAGE DeriveDataTypeable, FunctionalDependencies, KindSignatures #-}"
  ""
  "module Data.Hot.Base where"
  ""
  "import Data.Data (Data, Typeable)"
  "import GHC.TypeLits (Nat)"
  "\n"
  "class (Hot t n, Data (t a), Data a) => HotData t a n"
  ""
  TextM $ unlinesN n instanceHotData
  "\n"
  "class Hot (t :: * -> *) (n :: Nat) | t -> n, n -> t where"
  "\t" ++ "mapAt :: (a -> a) -> t a -> Int -> t a"
  "\n"
  TextM $ unlinesN1 n instanceHot
  "\n"
  TextM $ unlinesN1 n dataHot
  ""


instanceHotData, instanceHot, dataHot :: Int -> Text
instanceHotData n = "instance (Hot Hot" ++ show n +++ "n, Data a) => HotData Hot" ++ show n +++ "a n"

instanceHot n = "instance Hot Hot" ++ show n +++ show n +++ "where" ++>
  "\tmapAt f (" ++ hotConstr n (("x" ++) . show) ++ ") = \\case" ++>
  unlinesN n (mapAtCase n) ++>
  "\t\tn -> error $ \"Hot" ++ show n ++ " mapAt \" ++ show n"

mapAtCase :: Int -> Int -> Text
mapAtCase n i = "\t\t" ++ show (i - 1) +++ "->" +++ hotConstr n f where
  f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j

dataHot n =  "data Hot" ++ show n +++ "a =" +++ hotConstr n (const "!a") ++>
  "\tderiving (Eq, Ord, Read, Show, Data, Typeable)"

hotConstr :: Int -> (Int -> Text) -> Text
hotConstr n f = "T" ++ show n +++ unwords (map f [1 .. n])


unlinesN, unlinesN1 :: Int -> (Int -> Text) -> Text
unlinesN n f = intercalate "\n" $ map f [1 .. n]
unlinesN1 n f = intercalate "\n\n" $ map f [1 .. n]

(+++), (++>) :: Text -> Text -> Text
x +++ y = x ++ " " ++ y
x ++> y = x ++ "\n" ++ y


type TextM = TextMonad ()
newtype TextMonad a = TextM { runText :: Text }
  deriving (IsString, Monoid, Show)

instance Functor TextMonad where
  fmap = undefined

instance Applicative TextMonad where
  pure = undefined
  (<*>) = undefined

instance Monad TextMonad where
  (>>=) = undefined
  (TextM x) >> (TextM y) = TextM $ x ++ "\n" ++ y
