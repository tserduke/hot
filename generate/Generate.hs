module Main (main) where

import BasicPrelude
import qualified Data.Text as T


main :: IO ()
main = writeFile "../src/Data/Hot/Base.hs" (baseModule 10)


baseModule :: Int -> Text
baseModule n = runLines $ do
  "{-# LANGUAGE DeriveDataTypeable, FunctionalDependencies, KindSignatures, Rank2Types #-}"
  ""
  "module Data.Hot.Base where"
  ""
  "import Data.Data (Data, Typeable)"
  "import Data.Hot.Internal (hotError)"
  "import GHC.TypeLits (Nat)"
  "\n"
  "class (Hot t n, Data (t a), Data a) => HotData t a n"
  ""
  forN n instanceHotData
  "\n"
  "class Hot (t :: * -> *) (n :: Nat) | t -> n, n -> t where"
  tab 1 "unfold :: (forall r. c (a -> r) -> c r) -> (forall r. r -> c r) -> c (t a)"
  tab 1 "size :: t a -> Int"
  tab 1 "elementAt :: t a -> Int -> a"
  tab 1 "mapAt :: (a -> a) -> t a -> Int -> t a"
  "\n"
  forN n instanceHot
  ""
  forN n dataHot


instanceHotData, instanceHot, dataHot :: Int -> Line ()
instanceHotData n = Line $ "instance (Hot Hot" ++ show n +++ "n, Data a) => HotData Hot" ++ show n +++ "a n"

instanceHot n = do
  Line $ "instance Hot Hot" ++ show n +++ show n +++ "where"
  tab 1 $ "unfold f z =" +++ T.replicate n "f (" ++ "z Hot" ++ show n ++ T.replicate n ")"
  tab 1 $ "size _ =" +++ show n
  let constr = "(" ++ hotConstr n (("x" ++) . show) ++ ")"
  tab 1 $ "elementAt" +++ constr +++ "= \\case"
  forN n elementAtCase
  tab 2 $ "n -> hotError" +++ show n +++ "\"elementAt\" n"
  tab 1 $ "mapAt f" +++ constr +++ "= \\case"
  forN n (mapAtCase n)
  tab 2 $ "n -> hotError" +++ show n +++ "\"mapAt\" n"
  ""

elementAtCase :: Int -> Line ()
elementAtCase i = tab 2 $ show (i - 1) +++ "-> x" ++ show i

mapAtCase :: Int -> Int -> Line ()
mapAtCase n i = tab 2 $ show (i - 1) +++ "->" +++ hotConstr n f where
  f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j

dataHot n = do
  Line $ "data Hot" ++ show n +++ "a =" +++ hotConstr n (const "!a")
  tab 1 $ "deriving (Eq, Ord, Read, Show, Data, Typeable)"
  ""

hotConstr :: Int -> (Int -> Text) -> Text
hotConstr n f = "Hot" ++ show n +++ unwords (map f [1 .. n])


forN :: (Monad m) => Int -> (Int -> m a) -> m a
forN n f = foldr1 (>>) $ map f [1 .. n]

(+++) :: Text -> Text -> Text
x +++ y = x ++ " " ++ y

tab :: Int -> Text -> Line ()
tab n x = Line $ T.replicate n "\t" ++ x


newtype Line a = Line { runLines :: Text }
  deriving (IsString, Monoid)

instance Functor Line where
  fmap = undefined

instance Applicative Line where
  pure = undefined
  (<*>) = undefined

instance Monad Line where
  (>>=) = undefined
  (Line x) >> (Line y) = Line $ x ++ "\n" ++ y
