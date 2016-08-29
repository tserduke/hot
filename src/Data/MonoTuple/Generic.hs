module Data.MonoTuple.Generic where

import Data.Data (Data, fromConstr, gmapQi, toConstr)


{-# INLINE elementAt #-}
elementAt :: (Data a, Data b) => a -> Int -> b
elementAt x i = gmapQi i recast x


{-# INLINE recast #-}
recast :: (Data a, Data b) => a -> b
recast = fromConstr . toConstr
