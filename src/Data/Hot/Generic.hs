module Data.Hot.Generic
  ( prefix
  , suffix
  , merge
  ) where

import Data.Hot.Base
import GHC.TypeLits


{-# INLINABLE prefix #-}
prefix :: (Hot t n, Hot t1 m) => t a -> t1 a
prefix t = runSub $ unfold buildSub $ Sub 0 (elementAt t) where

{-# INLINABLE suffix #-}
suffix :: forall t n t1 m a. (Hot t n, Hot t1 m) => t a -> t1 a
suffix t = runSub $ unfold buildSub $ Sub (size t - size (undefined :: t1 a)) (elementAt t) where

data Sub a b = Sub !Int (Int -> a) b

buildSub :: Sub a (a -> r) -> Sub a r
buildSub (Sub i f k) = Sub (i + 1) f (k (f i))

runSub :: Sub f a -> a
runSub (Sub _ _ x) = x


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
