{-# LANGUAGE InstanceSigs, TypeFamilies, UndecidableInstances #-}

module Data.Hot.Sort
  ( Half
  , HotSort
  , sort
  ) where

import Data.Hot.Base
import Data.Hot.Instances
import Data.Hot.Generic
import GHC.TypeLits


class (HotClass n) => HotSort n where
  sort :: (Ord a) => Hot n a -> Hot n a


instance {-# OVERLAPPING #-} HotSort 1 where
  {-# INLINE sort #-}
  sort = id

instance {-# OVERLAPPING #-} HotSort 2 where
  {-# INLINABLE sort #-}
  sort t@(Hot2 x y) = if x > y
    then Hot2 y x
    else t

instance {-# OVERLAPPING #-} HotSort 3 where
  {-# INLINABLE sort #-}
  sort (Hot3 x y z) = if x < y
    then if | z > y     -> Hot3 x y z
            | z > x     -> Hot3 x z y
            | otherwise -> Hot3 z x y
    else if | z > x     -> Hot3 y x z
            | z > y     -> Hot3 y z x
            | otherwise -> Hot3 z y x

instance {-# OVERLAPPABLE #-} (
         HotClass n, n ~ (n1 + n2),
         HotClass n1, HotSort n1, n1 ~ Half n, n1 <= n,
         HotClass n2, HotSort n2, n2 ~ (n - n1), n2 <= n) => HotSort n where
  {-# INLINABLE sort #-}
  sort :: forall a. (Ord a) => Hot n a -> Hot n a
  sort t = merge (sort $ prefix t :: Hot n1 a) (sort $ suffix t :: Hot n2 a)


type family Half (n :: Nat) :: Nat where
  Half 1 = 1
  Half 2 = 1
  Half n = Half (n - 2) + 1
