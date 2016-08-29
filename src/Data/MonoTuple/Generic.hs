module Data.MonoTuple.Generic where

import Data.Data (Data, fromConstr, gmapQi, toConstr)


{-# INLINE elementAt #-}
elementAt :: (Data (t a), Data a) => t a -> Int -> a
elementAt = gelementAt

{-# INLINE gelementAt #-}
gelementAt :: (Data t, Data a) => t -> Int -> a
gelementAt x i = gmapQi i recast x


{-# INLINE recast #-}
recast :: (Data a, Data b) => a -> b
recast = fromConstr . toConstr
