module Data.Hot.Pidgin.Base where


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
