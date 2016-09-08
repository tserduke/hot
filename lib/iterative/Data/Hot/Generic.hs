{-# LANGUAGE TypeFamilies #-}

module Data.Hot.Generic
  ( prefix
  , suffix
  , merge
  ) where

import Data.Hot.Base
import Data.Hot.Iterator

import GHC.TypeLits


{-# INLINABLE prefix #-}
prefix :: (HotClass n, HotClass m, m <= n) => Hot n a -> Hot m a
prefix x = runSub $ unfold buildSub $ Sub 0 (elementAt x)

{-# INLINABLE suffix #-}
suffix :: forall n m a. (HotClass n, HotClass m, m <= n) => Hot n a -> Hot m a
suffix x = runSub $ unfold buildSub $ Sub from (elementAt x) where
  from = length x - length (undefined :: Hot m a)

data Sub a b = Sub !Int (Int -> a) b

buildSub :: Sub a (a -> r) -> Sub a r
buildSub (Sub i f k) = Sub (i + 1) f (k (f i))

runSub :: Sub a r -> r
runSub (Sub _ _ x) = x


{-# INLINABLE merge #-}
merge :: (HotClass n, HotClass m, HotClass (n + m), Ord a) => Hot n a -> Hot m a -> Hot (n + m) a
merge x y = runMerge $ unfold (buildMerge (length x) (length y)) (Merge 0 0 (iterRight x) (iterRight y))

data Merge a b = Merge !Int !Int (Iterator a) (Iterator a) b

buildMerge :: (Ord a) => Int -> Int -> Merge a (a -> r) -> Merge a r
buildMerge n m = \case
  (Merge i j x y k)
    | i == n                    -> take2
    | j == m                    -> take1
    | iterYield x < iterYield y -> take1
    | otherwise                 -> take2
    where
      take1 = Merge (i + 1) j (iterNext x) y (k $ iterYield x)
      take2 = Merge i (j + 1) x (iterNext y) (k $ iterYield y)

runMerge :: Merge a r -> r
runMerge (Merge _ _ _ _ x) = x