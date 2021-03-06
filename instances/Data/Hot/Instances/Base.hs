{-# LANGUAGE TypeFamilies #-}

module Data.Hot.Instances.Base where

import Data.Hot.Base
import Data.Hot.Internal (hotError)


instance HotBase 1 where
    data Hot 1 a = Hot1 !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (z Hot1)
    {-# INLINABLE elementAt #-}
    elementAt (Hot1 x1) = \case
        0 -> x1
        x -> hotError 1 "elementAt" x
    {-# INLINABLE mapAt #-}
    mapAt f (Hot1 x1) = \case
        0 -> Hot1 (f x1)
        x -> hotError 1 "mapAt" x

instance HotBase 2 where
    data Hot 2 a = Hot2 !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (z Hot2))
    {-# INLINABLE elementAt #-}
    elementAt (Hot2 x1 x2) = \case
        0 -> x1
        1 -> x2
        x -> hotError 2 "elementAt" x
    {-# INLINABLE mapAt #-}
    mapAt f (Hot2 x1 x2) = \case
        0 -> Hot2 (f x1) x2
        1 -> Hot2 x1 (f x2)
        x -> hotError 2 "mapAt" x

instance HotBase 3 where
    data Hot 3 a = Hot3 !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (z Hot3)))
    {-# INLINABLE elementAt #-}
    elementAt (Hot3 x1 x2 x3) = \case
        0 -> x1
        1 -> x2
        2 -> x3
        x -> hotError 3 "elementAt" x
    {-# INLINABLE mapAt #-}
    mapAt f (Hot3 x1 x2 x3) = \case
        0 -> Hot3 (f x1) x2 x3
        1 -> Hot3 x1 (f x2) x3
        2 -> Hot3 x1 x2 (f x3)
        x -> hotError 3 "mapAt" x

instance HotBase 4 where
    data Hot 4 a = Hot4 !a !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (f (z Hot4))))
    {-# INLINABLE elementAt #-}
    elementAt (Hot4 x1 x2 x3 x4) = \case
        0 -> x1
        1 -> x2
        2 -> x3
        3 -> x4
        x -> hotError 4 "elementAt" x
    {-# INLINABLE mapAt #-}
    mapAt f (Hot4 x1 x2 x3 x4) = \case
        0 -> Hot4 (f x1) x2 x3 x4
        1 -> Hot4 x1 (f x2) x3 x4
        2 -> Hot4 x1 x2 (f x3) x4
        3 -> Hot4 x1 x2 x3 (f x4)
        x -> hotError 4 "mapAt" x

instance HotBase 5 where
    data Hot 5 a = Hot5 !a !a !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (f (f (z Hot5)))))
    {-# INLINABLE elementAt #-}
    elementAt (Hot5 x1 x2 x3 x4 x5) = \case
        0 -> x1
        1 -> x2
        2 -> x3
        3 -> x4
        4 -> x5
        x -> hotError 5 "elementAt" x
    {-# INLINABLE mapAt #-}
    mapAt f (Hot5 x1 x2 x3 x4 x5) = \case
        0 -> Hot5 (f x1) x2 x3 x4 x5
        1 -> Hot5 x1 (f x2) x3 x4 x5
        2 -> Hot5 x1 x2 (f x3) x4 x5
        3 -> Hot5 x1 x2 x3 (f x4) x5
        4 -> Hot5 x1 x2 x3 x4 (f x5)
        x -> hotError 5 "mapAt" x

instance HotBase 6 where
    data Hot 6 a = Hot6 !a !a !a !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (f (f (f (z Hot6))))))
    {-# INLINABLE elementAt #-}
    elementAt (Hot6 x1 x2 x3 x4 x5 x6) = \case
        0 -> x1
        1 -> x2
        2 -> x3
        3 -> x4
        4 -> x5
        5 -> x6
        x -> hotError 6 "elementAt" x
    {-# INLINABLE mapAt #-}
    mapAt f (Hot6 x1 x2 x3 x4 x5 x6) = \case
        0 -> Hot6 (f x1) x2 x3 x4 x5 x6
        1 -> Hot6 x1 (f x2) x3 x4 x5 x6
        2 -> Hot6 x1 x2 (f x3) x4 x5 x6
        3 -> Hot6 x1 x2 x3 (f x4) x5 x6
        4 -> Hot6 x1 x2 x3 x4 (f x5) x6
        5 -> Hot6 x1 x2 x3 x4 x5 (f x6)
        x -> hotError 6 "mapAt" x

instance HotBase 7 where
    data Hot 7 a = Hot7 !a !a !a !a !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (f (f (f (f (z Hot7)))))))
    {-# INLINABLE elementAt #-}
    elementAt (Hot7 x1 x2 x3 x4 x5 x6 x7) = \case
        0 -> x1
        1 -> x2
        2 -> x3
        3 -> x4
        4 -> x5
        5 -> x6
        6 -> x7
        x -> hotError 7 "elementAt" x
    {-# INLINABLE mapAt #-}
    mapAt f (Hot7 x1 x2 x3 x4 x5 x6 x7) = \case
        0 -> Hot7 (f x1) x2 x3 x4 x5 x6 x7
        1 -> Hot7 x1 (f x2) x3 x4 x5 x6 x7
        2 -> Hot7 x1 x2 (f x3) x4 x5 x6 x7
        3 -> Hot7 x1 x2 x3 (f x4) x5 x6 x7
        4 -> Hot7 x1 x2 x3 x4 (f x5) x6 x7
        5 -> Hot7 x1 x2 x3 x4 x5 (f x6) x7
        6 -> Hot7 x1 x2 x3 x4 x5 x6 (f x7)
        x -> hotError 7 "mapAt" x

instance HotBase 8 where
    data Hot 8 a = Hot8 !a !a !a !a !a !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (f (f (f (f (f (z Hot8))))))))
    {-# INLINABLE elementAt #-}
    elementAt (Hot8 x1 x2 x3 x4 x5 x6 x7 x8) = \case
        0 -> x1
        1 -> x2
        2 -> x3
        3 -> x4
        4 -> x5
        5 -> x6
        6 -> x7
        7 -> x8
        x -> hotError 8 "elementAt" x
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
        x -> hotError 8 "mapAt" x

instance HotBase 9 where
    data Hot 9 a = Hot9 !a !a !a !a !a !a !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (f (f (f (f (f (f (z Hot9)))))))))
    {-# INLINABLE elementAt #-}
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
        x -> hotError 9 "elementAt" x
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
        x -> hotError 9 "mapAt" x

instance HotBase 10 where
    data Hot 10 a = Hot10 !a !a !a !a !a !a !a !a !a !a
        deriving (Eq, Ord, Read, Show)
    {-# INLINABLE unfold #-}
    unfold f z = f (f (f (f (f (f (f (f (f (f (z Hot10))))))))))
    {-# INLINABLE elementAt #-}
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
        x -> hotError 10 "elementAt" x
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
        x -> hotError 10 "mapAt" x

