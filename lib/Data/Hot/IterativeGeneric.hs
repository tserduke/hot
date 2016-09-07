module IterativeGeneric where

import GHC.TypeLits

import Data.Hot.Base
import Data.Hot.Iterator


{-# INLINABLE merge2 #-}
merge2 :: (HotClass n, HotClass m, HotClass (n + m), Ord a) => Hot n a -> Hot m a -> Hot (n + m) a
merge2 x y = runMerge2 $ unfold (buildMerge2 (length x) (length y)) (Merge2 0 0 (iterRight x) (iterRight y))

data Merge2 a b = Merge2 !Int !Int (Iterator a) (Iterator a) b

buildMerge2 :: (Ord a) => Int -> Int -> Merge2 a (a -> r) -> Merge2 a r
buildMerge2 n m = \case
  (Merge2 i j x y k)
    | i == n                    -> take2
    | j == m                    -> take1
    | iterYield x < iterYield y -> take1
    | otherwise                 -> take2
    where
      take1 = Merge2 (i + 1) j (iterNext x) y (k $ iterYield x)
      take2 = Merge2 i (j + 1) x (iterNext y) (k $ iterYield y)

runMerge2 :: Merge2 a r -> r
runMerge2 (Merge2 _ _ _ _ x) = x
