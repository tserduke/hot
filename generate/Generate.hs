module Main where

import BasicPrelude


main :: IO ()
main = writeFile "../src/Data/MonoTuple/Base.hs" (baseModule 10)


baseModule :: Int -> Text
baseModule n = runText $ do
  "{-# LANGUAGE DeriveDataTypeable, FunctionalDependencies, KindSignatures #-}"
  ""
  "module Data.MonoTuple.Base where"
  ""
  "import Data.Data (Data, Typeable)"
  "import GHC.TypeLits (Nat)"
  "\n"
  "class (Tuple t n, Data (t a), Data a) => TupleData t a n"
  ""
  TextM $ unlinesN n instanceTupleData
  "\n"
  "class Tuple (t :: * -> *) (n :: Nat) | t -> n, n -> t where"
  "\t" ++ "mapAt :: (a -> a) -> t a -> Int -> t a"
  "\n"
  TextM $ unlinesN1 n instanceTuple
  "\n"
  TextM $ unlinesN1 n dataTuple
  ""


instanceTupleData, instanceTuple, dataTuple :: Int -> Text
instanceTupleData n = "instance (Tuple Tuple" ++ show n +++ "n, Data a) => TupleData Tuple" ++ show n +++ "a n"

instanceTuple n = "instance Tuple Tuple" ++ show n +++ show n +++ "where" ++>
  "\tmapAt f (" ++ tupleConstr n (("x" ++) . show) ++ ") = \\case" ++>
  unlinesN n (mapAtCase n) ++>
  "\t\tn -> error $ \"Tuple" ++ show n ++ " mapAt \" ++ show n"

mapAtCase :: Int -> Int -> Text
mapAtCase n i = "\t\t" ++ show (i - 1) +++ "->" +++ tupleConstr n f where
  f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j

dataTuple n =  "data Tuple" ++ show n +++ "a =" +++ tupleConstr n (const "!a") ++>
  "\tderiving (Data, Typeable)"

tupleConstr :: Int -> (Int -> Text) -> Text
tupleConstr n f = "T" ++ show n +++ unwords (map f [1 .. n])


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
