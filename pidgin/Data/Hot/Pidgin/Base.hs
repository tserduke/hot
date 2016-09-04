{-# LANGUAGE TypeFamilies #-}

module Data.Hot.Pidgin.Base where

import GHC.Exts (IsList, Item, fromList, toList)


data PHot2 a = PHot2 !a !a
  deriving (Eq, Ord, Show)

data PHot3 a = PHot3 !a !a !a
  deriving (Eq, Ord, Show)

data PHot5 a = PHot5 !a !a !a !a !a
  deriving (Eq, Ord, Show)

data PHot10 a = PHot10 !a !a !a !a !a !a !a !a !a !a
  deriving (Eq, Ord, Show)


instance Functor PHot5 where
  fmap f (PHot5 x1 x2 x3 x4 x5) =
    PHot5 (f x1) (f x2) (f x3) (f x4) (f x5)

instance Functor PHot10 where
  fmap f (PHot10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) =
    PHot10 (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10)


instance IsList (PHot2 a) where
  type Item (PHot2 a) = a

  fromList (x1: x2: _) = PHot2 x1 x2
  fromList _ = error $ "PHot2 fromList"

  toList (PHot2 x1 x2) = [x1, x2]

instance IsList (PHot3 a) where
  type Item (PHot3 a) = a

  fromList (x1: x2: x3: _) = PHot3 x1 x2 x3
  fromList _ = error $ "PHot3 fromList"

  toList (PHot3 x1 x2 x3) = [x1, x2, x3]

instance IsList (PHot5 a) where
  type Item (PHot5 a) = a

  fromList (x1: x2: x3: x4: x5: _) = PHot5 x1 x2 x3 x4 x5
  fromList _ = error $ "PHot5 fromList"

  toList (PHot5 x1 x2 x3 x4 x5) = [x1, x2, x3, x4, x5]
