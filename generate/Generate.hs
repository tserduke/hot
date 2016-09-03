module Main (main) where

import BasicPrelude
import qualified Data.Text as T


main :: IO ()
main = do
  let n = 10
  writeFile "inline/Data/Hot/Base.hs" (baseModule inlinePragma n)
  writeFile "inlinable/Data/Hot/Base.hs" (baseModule inlinablePragma n)


baseModule :: (Text -> Text) -> Int -> Text
baseModule inline n = runLines $ do
  "{-# LANGUAGE KindSignatures, Rank2Types, TypeFamilies #-}"
  ""
  "module Data.Hot.Base where"
  ""
  "import Data.Hot.Internal (hotError)"
  "import GHC.TypeLits (Nat)"
  "\n"
  "class HotClass (n :: Nat) where"
  tab 1 "data Hot n :: * -> *"
  tab 1 "unfold :: (forall r. c (a -> r) -> c r) -> (forall r. r -> c r) -> c (Hot n a)"
  tab 1 "size :: Hot n a -> Int"
  tab 1 "elementAt :: Hot n a -> Int -> a"
  tab 1 "mapAt :: (a -> a) -> Hot n a -> Int -> Hot n a"
  "\n"
  forN n (instanceHot inline)

inlinePragma, inlinablePragma :: Text -> Text
inlinePragma func = "{-# INLINE" +++ func +++ "#-}"
inlinablePragma func = "{-# INLINABLE" +++ func +++ "#-}"


instanceHot :: (Text -> Text) -> Int -> Line ()
instanceHot inline n = do
  Line $ "instance HotClass" +++ show n +++ "where"
  dataHot n
  tab 1 $ inline "unfold"
  tab 1 $ "unfold f z =" +++ T.replicate n "f (" ++ "z Hot" ++ show n ++ T.replicate n ")"
  tab 1 $ "{-# INLINE size #-}"
  tab 1 $ "size _ =" +++ show n
  let constr = "(" ++ hotConstr n (("x" ++) . show) ++ ")"
  tab 1 $ inline "elementAt"
  tab 1 $ "elementAt" +++ constr +++ "= \\case"
  forN n elementAtCase
  tab 2 $ "n -> hotError" +++ show n +++ "\"elementAt\" n"
  tab 1 $ inline "mapAt"
  tab 1 $ "mapAt f" +++ constr +++ "= \\case"
  forN n (mapAtCase n)
  tab 2 $ "n -> hotError" +++ show n +++ "\"mapAt\" n"
  ""

dataHot, elementAtCase :: Int -> Line ()
dataHot n = do
  tab 1 $ "data Hot" +++ show n +++ "a =" +++ hotConstr n (const "!a")
  tab 2 $ "deriving (Eq, Ord, Read, Show)"

elementAtCase i = tab 2 $ show (i - 1) +++ "-> x" ++ show i

mapAtCase :: Int -> Int -> Line ()
mapAtCase n i = tab 2 $ show (i - 1) +++ "->" +++ hotConstr n f where
  f j = if j == i then "(f x" ++ show j ++ ")" else "x" ++ show j

hotConstr :: Int -> (Int -> Text) -> Text
hotConstr n f = "Hot" ++ show n +++ unwords (map f [1 .. n])


forN :: (Monad m) => Int -> (Int -> m a) -> m a
forN n f = foldr1 (>>) $ map f [1 .. n]

(+++) :: Text -> Text -> Text
x +++ y = x ++ " " ++ y

tab :: Int -> Text -> Line ()
tab n x = Line $ T.replicate n "    " ++ x


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
