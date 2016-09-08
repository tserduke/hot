{-# LANGUAGE KindSignatures, Rank2Types, TypeFamilies #-}

module Data.Hot.Base
  ( HotClass
  , Hot
  , unfold
  , elementAt
  , mapAt
  ) where

import GHC.TypeLits (Nat)


class (Foldable (Hot n)) => HotClass (n :: Nat) where
    data Hot n :: * -> *
    unfold :: (forall r. c (a -> r) -> c r) -> (forall r. r -> c r) -> c (Hot n a)
    elementAt :: Hot n a -> Int -> a
    mapAt :: (a -> a) -> Hot n a -> Int -> Hot n a
