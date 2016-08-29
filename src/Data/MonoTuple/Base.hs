{-# LANGUAGE FunctionalDependencies, KindSignatures #-}

module Data.MonoTuple.Base where

import GHC.TypeLits (Nat)


class Tuple (t :: * -> *) (n :: Nat) | t -> n, n -> t

instance Tuple Tuple1 1
instance Tuple Tuple2 2
instance Tuple Tuple3 3
instance Tuple Tuple4 4
instance Tuple Tuple5 5
instance Tuple Tuple6 6
instance Tuple Tuple7 7
instance Tuple Tuple8 8
instance Tuple Tuple9 9
instance Tuple Tuple10 10


data Tuple1 a = T1 !a
data Tuple2 a = T2 !a !a
data Tuple3 a = T3 !a !a !a
data Tuple4 a = T4 !a !a !a !a
data Tuple5 a = T5 !a !a !a !a !a
data Tuple6 a = T6 !a !a !a !a !a !a
data Tuple7 a = T7 !a !a !a !a !a !a !a
data Tuple8 a = T8 !a !a !a !a !a !a !a !a
data Tuple9 a = T9 !a !a !a !a !a !a !a !a !a
data Tuple10 a = T10 !a !a !a !a !a !a !a !a !a !a
