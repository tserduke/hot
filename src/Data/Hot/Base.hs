{-# LANGUAGE KindSignatures, Rank2Types, TypeFamilies #-}

module Data.Hot.Base where

import Data.Hot.Internal (hotError)
import GHC.TypeLits (Nat)


class HotClass (n :: Nat) where
   data Hot n :: * -> *
   unfold :: (forall r. c (a -> r) -> c r) -> (forall r. r -> c r) -> c (Hot n a)
   size :: Hot n a -> Int
   elementAt :: Hot n a -> Int -> a
   mapAt :: (a -> a) -> Hot n a -> Int -> Hot n a


instance HotClass 1 where
   data Hot 1 a = Hot1 !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (z Hot1)
   {-# INLINE size #-}
   size _ = 1
   {-# INLINE elementAt #-}
   elementAt (Hot1 x1) = \case
      0 -> x1
      n -> hotError 1 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot1 x1) = \case
      0 -> Hot1 (f x1)
      n -> hotError 1 "mapAt" n

instance HotClass 2 where
   data Hot 2 a = Hot2 !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (z Hot2))
   {-# INLINE size #-}
   size _ = 2
   {-# INLINE elementAt #-}
   elementAt (Hot2 x1 x2) = \case
      0 -> x1
      1 -> x2
      n -> hotError 2 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot2 x1 x2) = \case
      0 -> Hot2 (f x1) x2
      1 -> Hot2 x1 (f x2)
      n -> hotError 2 "mapAt" n

instance HotClass 3 where
   data Hot 3 a = Hot3 !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (z Hot3)))
   {-# INLINE size #-}
   size _ = 3
   {-# INLINE elementAt #-}
   elementAt (Hot3 x1 x2 x3) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      n -> hotError 3 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot3 x1 x2 x3) = \case
      0 -> Hot3 (f x1) x2 x3
      1 -> Hot3 x1 (f x2) x3
      2 -> Hot3 x1 x2 (f x3)
      n -> hotError 3 "mapAt" n

instance HotClass 4 where
   data Hot 4 a = Hot4 !a !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (f (z Hot4))))
   {-# INLINE size #-}
   size _ = 4
   {-# INLINE elementAt #-}
   elementAt (Hot4 x1 x2 x3 x4) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      3 -> x4
      n -> hotError 4 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot4 x1 x2 x3 x4) = \case
      0 -> Hot4 (f x1) x2 x3 x4
      1 -> Hot4 x1 (f x2) x3 x4
      2 -> Hot4 x1 x2 (f x3) x4
      3 -> Hot4 x1 x2 x3 (f x4)
      n -> hotError 4 "mapAt" n

instance HotClass 5 where
   data Hot 5 a = Hot5 !a !a !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (f (f (z Hot5)))))
   {-# INLINE size #-}
   size _ = 5
   {-# INLINE elementAt #-}
   elementAt (Hot5 x1 x2 x3 x4 x5) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      3 -> x4
      4 -> x5
      n -> hotError 5 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot5 x1 x2 x3 x4 x5) = \case
      0 -> Hot5 (f x1) x2 x3 x4 x5
      1 -> Hot5 x1 (f x2) x3 x4 x5
      2 -> Hot5 x1 x2 (f x3) x4 x5
      3 -> Hot5 x1 x2 x3 (f x4) x5
      4 -> Hot5 x1 x2 x3 x4 (f x5)
      n -> hotError 5 "mapAt" n

instance HotClass 6 where
   data Hot 6 a = Hot6 !a !a !a !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (f (f (f (z Hot6))))))
   {-# INLINE size #-}
   size _ = 6
   {-# INLINE elementAt #-}
   elementAt (Hot6 x1 x2 x3 x4 x5 x6) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      3 -> x4
      4 -> x5
      5 -> x6
      n -> hotError 6 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot6 x1 x2 x3 x4 x5 x6) = \case
      0 -> Hot6 (f x1) x2 x3 x4 x5 x6
      1 -> Hot6 x1 (f x2) x3 x4 x5 x6
      2 -> Hot6 x1 x2 (f x3) x4 x5 x6
      3 -> Hot6 x1 x2 x3 (f x4) x5 x6
      4 -> Hot6 x1 x2 x3 x4 (f x5) x6
      5 -> Hot6 x1 x2 x3 x4 x5 (f x6)
      n -> hotError 6 "mapAt" n

instance HotClass 7 where
   data Hot 7 a = Hot7 !a !a !a !a !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (f (f (f (f (z Hot7)))))))
   {-# INLINE size #-}
   size _ = 7
   {-# INLINE elementAt #-}
   elementAt (Hot7 x1 x2 x3 x4 x5 x6 x7) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      3 -> x4
      4 -> x5
      5 -> x6
      6 -> x7
      n -> hotError 7 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot7 x1 x2 x3 x4 x5 x6 x7) = \case
      0 -> Hot7 (f x1) x2 x3 x4 x5 x6 x7
      1 -> Hot7 x1 (f x2) x3 x4 x5 x6 x7
      2 -> Hot7 x1 x2 (f x3) x4 x5 x6 x7
      3 -> Hot7 x1 x2 x3 (f x4) x5 x6 x7
      4 -> Hot7 x1 x2 x3 x4 (f x5) x6 x7
      5 -> Hot7 x1 x2 x3 x4 x5 (f x6) x7
      6 -> Hot7 x1 x2 x3 x4 x5 x6 (f x7)
      n -> hotError 7 "mapAt" n

instance HotClass 8 where
   data Hot 8 a = Hot8 !a !a !a !a !a !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (f (f (f (f (f (z Hot8))))))))
   {-# INLINE size #-}
   size _ = 8
   {-# INLINE elementAt #-}
   elementAt (Hot8 x1 x2 x3 x4 x5 x6 x7 x8) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      3 -> x4
      4 -> x5
      5 -> x6
      6 -> x7
      7 -> x8
      n -> hotError 8 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot8 x1 x2 x3 x4 x5 x6 x7 x8) = \case
      0 -> Hot8 (f x1) x2 x3 x4 x5 x6 x7 x8
      1 -> Hot8 x1 (f x2) x3 x4 x5 x6 x7 x8
      2 -> Hot8 x1 x2 (f x3) x4 x5 x6 x7 x8
      3 -> Hot8 x1 x2 x3 (f x4) x5 x6 x7 x8
      4 -> Hot8 x1 x2 x3 x4 (f x5) x6 x7 x8
      5 -> Hot8 x1 x2 x3 x4 x5 (f x6) x7 x8
      6 -> Hot8 x1 x2 x3 x4 x5 x6 (f x7) x8
      7 -> Hot8 x1 x2 x3 x4 x5 x6 x7 (f x8)
      n -> hotError 8 "mapAt" n

instance HotClass 9 where
   data Hot 9 a = Hot9 !a !a !a !a !a !a !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (f (f (f (f (f (f (z Hot9)))))))))
   {-# INLINE size #-}
   size _ = 9
   {-# INLINE elementAt #-}
   elementAt (Hot9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      3 -> x4
      4 -> x5
      5 -> x6
      6 -> x7
      7 -> x8
      8 -> x9
      n -> hotError 9 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = \case
      0 -> Hot9 (f x1) x2 x3 x4 x5 x6 x7 x8 x9
      1 -> Hot9 x1 (f x2) x3 x4 x5 x6 x7 x8 x9
      2 -> Hot9 x1 x2 (f x3) x4 x5 x6 x7 x8 x9
      3 -> Hot9 x1 x2 x3 (f x4) x5 x6 x7 x8 x9
      4 -> Hot9 x1 x2 x3 x4 (f x5) x6 x7 x8 x9
      5 -> Hot9 x1 x2 x3 x4 x5 (f x6) x7 x8 x9
      6 -> Hot9 x1 x2 x3 x4 x5 x6 (f x7) x8 x9
      7 -> Hot9 x1 x2 x3 x4 x5 x6 x7 (f x8) x9
      8 -> Hot9 x1 x2 x3 x4 x5 x6 x7 x8 (f x9)
      n -> hotError 9 "mapAt" n

instance HotClass 10 where
   data Hot 10 a = Hot10 !a !a !a !a !a !a !a !a !a !a
      deriving (Eq, Ord, Read, Show)
   {-# INLINE unfold #-}
   unfold f z = f (f (f (f (f (f (f (f (f (f (z Hot10))))))))))
   {-# INLINE size #-}
   size _ = 10
   {-# INLINE elementAt #-}
   elementAt (Hot10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = \case
      0 -> x1
      1 -> x2
      2 -> x3
      3 -> x4
      4 -> x5
      5 -> x6
      6 -> x7
      7 -> x8
      8 -> x9
      9 -> x10
      n -> hotError 10 "elementAt" n
   {-# INLINABLE mapAt #-}
   mapAt f (Hot10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = \case
      0 -> Hot10 (f x1) x2 x3 x4 x5 x6 x7 x8 x9 x10
      1 -> Hot10 x1 (f x2) x3 x4 x5 x6 x7 x8 x9 x10
      2 -> Hot10 x1 x2 (f x3) x4 x5 x6 x7 x8 x9 x10
      3 -> Hot10 x1 x2 x3 (f x4) x5 x6 x7 x8 x9 x10
      4 -> Hot10 x1 x2 x3 x4 (f x5) x6 x7 x8 x9 x10
      5 -> Hot10 x1 x2 x3 x4 x5 (f x6) x7 x8 x9 x10
      6 -> Hot10 x1 x2 x3 x4 x5 x6 (f x7) x8 x9 x10
      7 -> Hot10 x1 x2 x3 x4 x5 x6 x7 (f x8) x9 x10
      8 -> Hot10 x1 x2 x3 x4 x5 x6 x7 x8 (f x9) x10
      9 -> Hot10 x1 x2 x3 x4 x5 x6 x7 x8 x9 (f x10)
      n -> hotError 10 "mapAt" n
