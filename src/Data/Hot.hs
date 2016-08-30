module Data.Hot
  ( module Base
  , module TypeLits
  , elementAt
  , merge
  ) where

import Data.Data (toConstr)
import Data.Hot.Base as Base
import qualified Data.Hot.Generic as G
import GHC.TypeLits as TypeLits


elementAt :: (HotData t a n) => t a -> Int -> a
elementAt = G.elementAt


merge :: forall t1 t2 t a n m. (HotData t1 a n, HotData t2 a m, HotData t a (n + m), Ord a) => t1 a -> t2 a -> t a
merge x y = G.merge (toConstr (undefined :: t a)) (size x) (size y) x y
