module Data.MonoTuple
  ( module Base
  , module TypeLits
  , elementAt
  , merge
  ) where

import Data.MonoTuple.Base as Base
import qualified Data.MonoTuple.Generic as G
import GHC.TypeLits as TypeLits


elementAt :: (TupleData t a n) => t a -> Int -> a
elementAt = G.elementAt


merge :: (TupleData t1 a n, TupleData t2 a m, TupleData t a (n + m), Ord a) => t1 a -> t2 a -> t a
merge = G.merge undefined
