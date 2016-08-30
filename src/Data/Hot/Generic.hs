module Data.Hot.Generic
  ( merge
  ) where

import Data.Hot.Base
import GHC.TypeLits


merge ::  (HotData t1 a n, HotData t2 a m, HotData t a (n + m), Ord a) => t1 a -> t2 a -> t a
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
