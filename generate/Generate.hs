module Main where

import BasicPrelude


main :: IO ()
main = writeFile "../src/Data/MonoTuple/Base.hs" (runText $ baseModule 10)


baseModule :: Int -> TextM
baseModule n = do
  "{-# LANGUAGE FunctionalDependencies, KindSignatures #-}"
  ""
  "module Data.MonoTuple.Base where"
  ""
  "import GHC.TypeLits (Nat)"
  "\n"
  "class Tuple (t :: * -> *) (n :: Nat) | t -> n, n -> t"
  ""
  definitions n instanceTuple
  ""
  definitions n dataTuple


instanceTuple, dataTuple :: Int -> [Text]
instanceTuple n = ["instance Tuple Tuple", show n, " ", show n]

dataTuple n = ["data Tuple", show n, " a = T", show n, " ", unwords $ take n $ repeat "!a"]


definitions :: Int -> (Int -> [Text]) -> TextM
definitions n def = TextM $ unlines $ map (mconcat . def) [1 .. n]


type TextM = TextMonad ()
newtype TextMonad a = TextM { runText :: Text }
  deriving (IsString, Show)

instance Functor TextMonad where
  fmap = undefined

instance Applicative TextMonad where
  pure = undefined
  (<*>) = undefined

instance Monad TextMonad where
  (>>=) = undefined
  (TextM x) >> (TextM y) = TextM $ x ++ "\n" ++ y
