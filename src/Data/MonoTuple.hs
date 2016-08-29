module Data.MonoTuple
  ( module Base
  , elementAt
  ) where

import Data.Data (Data)
import Data.MonoTuple.Base as Base
import qualified Data.MonoTuple.Generic as G


elementAt :: (Data (t a), Data a) => t a -> Int -> a
elementAt = G.elementAt
