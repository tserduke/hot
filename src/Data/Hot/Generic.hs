module Data.Hot.Generic
  ( prefix
  , suffix
  , merge
  ) where

import Data.Hot.Base
import GHC.TypeLits


{-# INLINABLE prefix #-}
prefix :: (Hot t n, Hot t1 m) => t a -> t1 a
prefix t = runSub $ unfold f (Sub 0) where
  f (Sub i k) = Sub (i + 1) (k (elementAt t i))

{-# INLINABLE suffix #-}
suffix :: forall t n t1 m a. (Hot t n, Hot t1 m) => t a -> t1 a
suffix t = runSub $ unfold f (Sub (size t - size (undefined :: t1 a))) where
  f (Sub i k) = Sub (i + 1) (k (elementAt t i))

data Sub a = Sub !Int a
runSub :: Sub a -> a
runSub (Sub _ x) = x


{-# INLINABLE merge #-}
merge :: (Hot t (n + m), Hot t1 n, Hot t2 m, Ord a) => t1 a -> t2 a -> t a
merge x y = runMerge $ unfold f (M 0 0 (elementAt x) (elementAt y)) where
  f (M i j g p k) | i == size x = take2
                  | j == size y = take1
                  | p j > g i = take1
                  | otherwise = take2
                  where
                    take1 = M (i + 1) j g p (k $ g i)
                    take2 = M i (j + 1) g p (k $ p j)

data Merge a b c = M !Int !Int a b c

runMerge :: Merge a b c -> c
runMerge (M _ _ _ _ x) = x
