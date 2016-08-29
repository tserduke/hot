module Data.MonoTuple.Generic where

import Data.Data (Data, fromConstr, gmapQi, toConstr)


{-# INLINE elementAt #-}
elementAt :: (Data t, Data a) => t -> Int -> a
elementAt x i = gmapQi i recast x

{-# INLINE elementAt1 #-}
elementAt1 :: (Data (t a), Data a) => t a -> Int -> a
elementAt1 = elementAt


{-# INLINE recast #-}
recast :: (Data a, Data b) => a -> b
recast = fromConstr . toConstr
