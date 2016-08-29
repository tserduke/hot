module Data.MonoTuple.Generic where

import Data.Data (Constr, Data, fromConstr, gmapQi, toConstr)


recast :: (Data a, Data b) => a -> b
recast = fromConstr . toConstr


elementAt :: (Data a, Data b) => a -> Int -> b
elementAt x i = gmapQi i recast x


merge :: (Data a, Data b, Data c) => Constr -> a -> b -> c
merge = undefined
