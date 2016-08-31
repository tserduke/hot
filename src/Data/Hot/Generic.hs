{-# LANGUAGE TypeFamilies #-}

module Data.Hot.Generic
  ( prefix
  , suffix
  , merge
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


{-# INLINABLE merge #-}
merge :: (Hot t (n + m), Hot t1 n, Hot t2 m, Ord a) => t1 a -> t2 a -> t a
merge x y = runMerge $ unfold (buildMerge (size x) (size y)) (Merge 0 0 (elementAt x) (elementAt y))

data Merge a b = Merge !Int !Int (Int -> a) (Int -> a) b

buildMerge :: (Ord a) => Int -> Int -> Merge a (a -> r) -> Merge a r
buildMerge n m = \case
  (Merge i j f g k)
    | i == n    -> take2
    | j == m    -> take1
    | f i < g j -> take1
    | otherwise -> take2
    where
      take1 = Merge (i + 1) j f g (k (f i))
      take2 = Merge i (j + 1) f g (k (g j))

runMerge :: Merge a r -> r
runMerge (Merge _ _ _ _ x) = x
