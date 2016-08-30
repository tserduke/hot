module Data.MonoTuple
  ( module Base
  , module TypeLits
  , elementAt
  , merge
  ) where

import Data.Data (toConstr)
import Data.MonoTuple.Base as Base
import qualified Data.MonoTuple.Generic as G
import GHC.TypeLits as TypeLits


elementAt :: (TupleData t a n) => t a -> Int -> a
elementAt = G.elementAt


merge :: forall t1 t2 t a n m. (TupleData t1 a n, TupleData t2 a m, TupleData t a (n + m), Ord a) => t1 a -> t2 a -> t a
merge x y = G.merge (toConstr (undefined :: t a)) (size x) (size y) x y
