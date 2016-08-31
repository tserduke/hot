{-# LANGUAGE TypeFamilies #-}

module Data.Hot.Generic
  ( prefix
  , suffix
  , merge
  , merge2
  ) where

import Data.Hot.Base
import GHC.TypeLits


{-# INLINABLE prefix #-}
prefix :: (Hot t n, Hot t1 m, m <= n) => t a -> t1 a
prefix t = runSub $ unfold buildSub $ Sub 0 (elementAt t)

{-# INLINABLE suffix #-}
suffix :: forall t n t1 m a. (Hot t n, Hot t1 m, m <= n) => t a -> t1 a
suffix t = runSub $ unfold buildSub $ Sub from (elementAt t) where
  from = size t - size (undefined :: t1 a)

data Sub a b = Sub !Int (Int -> a) b

buildSub :: Sub a (a -> r) -> Sub a r
buildSub (Sub i f k) = Sub (i + 1) f (k (f i))

runSub :: Sub a r -> r
runSub (Sub _ _ x) = x


{-# INLINABLE merge2 #-}
merge2 :: (Hot t (n + m), Hot t1 n, Hot t2 m, Ord a) => t1 a -> t2 a -> t a
merge2 x y = runMerge2 $ unfold (buildMerge2 (size x - 1) (size y - 1)) (M2 (MB 0 (f 0) f) (MB 0 (g 0) g)) where
  f = elementAt x
  g = elementAt y

data M2 a b = M2 (MB a) (MB a) b | M1 (Sub a b)
data MB a = MB !Int !a (Int -> a)

buildMerge2 :: (Ord a) => Int -> Int -> M2 a (a -> r) -> M2 a r
buildMerge2 n m = \case
  M1 sub -> M1 $ buildSub sub
  M2 a@(MB i x f) b@(MB j y g) k -> if x < y
    then if i == n
         then M1 $ Sub j g (k x)
         else M2 (MB (i + 1) (f (i + 1)) f) b (k x)
    else if j == m
         then M1 $ Sub i f (k y)
         else M2 a (MB (j + 1) (g (j + 1)) g) (k y)

runMerge2 :: M2 a r -> r
runMerge2 (M1 sub) = runSub sub
runMerge2 _ = error "Impossible hot merge!!!"


{-# INLINABLE merge #-}
merge :: (Hot t (n + m), Hot t1 n, Hot t2 m, Ord a) => t1 a -> t2 a -> t a
merge x y = runMerge $ unfold (buildMerge (size x) (size y)) (M 0 0 (elementAt x) (elementAt y))

data Merge a b = M !Int !Int (Int -> a) (Int -> a) b

buildMerge :: (Ord a) => Int -> Int -> Merge a (a -> r) -> Merge a r
buildMerge n m (M i j g p k)
  | i == n = take2
  | j == m = take1
  | p j > g i = take1
  | otherwise = take2
  where
    take1 = M (i + 1) j g p (k $ g i)
    take2 = M i (j + 1) g p (k $ p j)

runMerge :: Merge a r -> r
runMerge (M _ _ _ _ x) = x
