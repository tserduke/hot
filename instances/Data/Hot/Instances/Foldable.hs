module Data.Hot.Instances.Foldable where

import Data.Hot.Instances.Base


instance Foldable (Hot 1) where
    {-# INLINE length #-}
    length _ = 1
    {-# INLINABLE foldr #-}
    foldr f z (Hot1 x1) = f x1 z

instance Foldable (Hot 2) where
    {-# INLINE length #-}
    length _ = 2
    {-# INLINABLE foldr #-}
    foldr f z (Hot2 x1 x2) = f x1 (f x2 z)

instance Foldable (Hot 3) where
    {-# INLINE length #-}
    length _ = 3
    {-# INLINABLE foldr #-}
    foldr f z (Hot3 x1 x2 x3) = f x1 (f x2 (f x3 z))

instance Foldable (Hot 4) where
    {-# INLINE length #-}
    length _ = 4
    {-# INLINABLE foldr #-}
    foldr f z (Hot4 x1 x2 x3 x4) = f x1 (f x2 (f x3 (f x4 z)))

instance Foldable (Hot 5) where
    {-# INLINE length #-}
    length _ = 5
    {-# INLINABLE foldr #-}
    foldr f z (Hot5 x1 x2 x3 x4 x5) = f x1 (f x2 (f x3 (f x4 (f x5 z))))

instance Foldable (Hot 6) where
    {-# INLINE length #-}
    length _ = 6
    {-# INLINABLE foldr #-}
    foldr f z (Hot6 x1 x2 x3 x4 x5 x6) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 z)))))

instance Foldable (Hot 7) where
    {-# INLINE length #-}
    length _ = 7
    {-# INLINABLE foldr #-}
    foldr f z (Hot7 x1 x2 x3 x4 x5 x6 x7) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 z))))))

instance Foldable (Hot 8) where
    {-# INLINE length #-}
    length _ = 8
    {-# INLINABLE foldr #-}
    foldr f z (Hot8 x1 x2 x3 x4 x5 x6 x7 x8) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 z)))))))

instance Foldable (Hot 9) where
    {-# INLINE length #-}
    length _ = 9
    {-# INLINABLE foldr #-}
    foldr f z (Hot9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 z))))))))

instance Foldable (Hot 10) where
    {-# INLINE length #-}
    length _ = 10
    {-# INLINABLE foldr #-}
    foldr f z (Hot10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = f x1 (f x2 (f x3 (f x4 (f x5 (f x6 (f x7 (f x8 (f x9 (f x10 z)))))))))

