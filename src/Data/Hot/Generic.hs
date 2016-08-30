module Data.Hot.Generic
  ( recast
  , elementAt
  , merge
  ) where

import Data.Data (Constr, Data, fromConstr, gmapQi, gunfold, toConstr)


recast :: (Data a, Data b) => a -> b
recast = fromConstr . toConstr


elementAt :: (Data a, Data b) => a -> Int -> b
elementAt x i = gmapQi i recast x


merge :: forall t1 t2 t a. (Data (t1 a), Data (t2 a), Data (t a), Data a, Ord a) => Constr -> Int -> Int -> t1 a -> t2 a -> t a
merge constr n m t1 t2 = runMerge $ gunfold f (M 0 0 (elementAt t1 :: Int -> a) (elementAt t2)) constr where
  f (M i j g p k) | i == n = take2
                  | j == m = take1
                  | p j > g i = take1
                  | otherwise = take2
                  where
                    take1 = M (i + 1) j g p (k $ recast $ g i)
                    take2 = M i (j + 1) g p (k $ recast $ p j)

data Merge a b c = M !Int !Int a b c

runMerge :: Merge a b c -> c
runMerge (M _ _ _ _ x) = x
