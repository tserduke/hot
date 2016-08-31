{-# LANGUAGE InstanceSigs, TypeFamilies, UndecidableInstances #-}

module Data.Hot.Sort
  ( Half
  , Sort
  , sort
  ) where

import Data.Hot.Base
import Data.Hot.Generic
import GHC.TypeLits


class (Hot t (HotNat t)) => Sort t where
  sort :: (Ord a) => t a -> t a


instance Sort Hot1 where
  {-# INLINE sort #-}
  sort = id

instance Sort Hot2 where
  {-# INLINABLE sort #-}
  sort t@(Hot2 x y) = if x > y
    then Hot2 y x
    else t

instance (Hot t n, n ~ HotNat t, n ~ (n1 + n2),
          Hot t1 n1, Sort t1, t1 ~ HotType n1, n1 ~ Half n,
          Hot t2 n2, Sort t2, t2 ~ HotType n2, n2 ~ (n - n1)) => Sort t where
  {-# INLINABLE sort #-}
  sort :: forall a. (Ord a) => t a -> t a
  sort t = merge (sort $ prefix t :: t1 a) (sort $ suffix t :: t2 a)


type family Half (n :: Nat) :: Nat where
  Half 1 = 1
  Half 2 = 1
  Half n = Half (n - 2) + 1
