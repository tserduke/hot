module Data.MonoTuple.Generic
  ( recast
  , elementAt
  , merge
  ) where

import Data.Data (Constr, Data, fromConstr, gmapQi, gunfold, toConstr)


recast :: (Data a, Data b) => a -> b
recast = fromConstr . toConstr


elementAt :: (Data a, Data b) => a -> Int -> b
elementAt x i = gmapQi i recast x


merge :: (Data a, Data b, Data c) => Constr -> a -> b -> c
merge constr t1 t2 = runMerge $ gunfold f (M 0 0 (elementAt t1) (elementAt t2)) constr where
  f (M i j g p k) | i == 2 = take2
                  | j == 3 = take1
                  | p j > g i = take1
                  | otherwise = take2
                  where
                    take1 = M (i + 1) j g p (k $ recast $ g i)
                    take2 = M i (j + 1) g p (k $ recast $ p j)

data Merge a b c = M !Int !Int a b c

runMerge :: Merge a b c -> c
runMerge (M _ _ _ _ x) = x
