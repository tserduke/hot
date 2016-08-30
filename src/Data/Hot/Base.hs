{-# LANGUAGE DeriveDataTypeable, FunctionalDependencies, KindSignatures #-}

module Data.Hot.Base where

import Data.Data (Data, Typeable)
import Data.Hot.Internal (hotError)
import GHC.TypeLits (Nat)


class (Hot t n, Data (t a), Data a) => HotData t a n

instance (Hot Hot1 n, Data a) => HotData Hot1 a n
instance (Hot Hot2 n, Data a) => HotData Hot2 a n
instance (Hot Hot3 n, Data a) => HotData Hot3 a n
instance (Hot Hot4 n, Data a) => HotData Hot4 a n
instance (Hot Hot5 n, Data a) => HotData Hot5 a n
instance (Hot Hot6 n, Data a) => HotData Hot6 a n
instance (Hot Hot7 n, Data a) => HotData Hot7 a n
instance (Hot Hot8 n, Data a) => HotData Hot8 a n
instance (Hot Hot9 n, Data a) => HotData Hot9 a n
instance (Hot Hot10 n, Data a) => HotData Hot10 a n


class Hot (t :: * -> *) (n :: Nat) | t -> n, n -> t where
	size :: t a -> Int
	elementAt :: t a -> Int -> a
	mapAt :: (a -> a) -> t a -> Int -> t a


instance Hot Hot1 1 where
	size _ = 1
	elementAt (Hot1 x1) = \case
		0 -> x1
		n -> hotError 1 "elementAt" n
	mapAt f (Hot1 x1) = \case
		0 -> Hot1 (f x1)
		n -> hotError 1 "mapAt" n

instance Hot Hot2 2 where
	size _ = 2
	elementAt (Hot2 x1 x2) = \case
		0 -> x1
		1 -> x2
		n -> hotError 2 "elementAt" n
	mapAt f (Hot2 x1 x2) = \case
		0 -> Hot2 (f x1) x2
		1 -> Hot2 x1 (f x2)
		n -> hotError 2 "mapAt" n

instance Hot Hot3 3 where
	size _ = 3
	elementAt (Hot3 x1 x2 x3) = \case
		0 -> x1
		1 -> x2
		2 -> x3
		n -> hotError 3 "elementAt" n
	mapAt f (Hot3 x1 x2 x3) = \case
		0 -> Hot3 (f x1) x2 x3
		1 -> Hot3 x1 (f x2) x3
		2 -> Hot3 x1 x2 (f x3)
		n -> hotError 3 "mapAt" n

instance Hot Hot4 4 where
	size _ = 4
	elementAt (Hot4 x1 x2 x3 x4) = \case
		0 -> x1
		1 -> x2
		2 -> x3
		3 -> x4
		n -> hotError 4 "elementAt" n
	mapAt f (Hot4 x1 x2 x3 x4) = \case
		0 -> Hot4 (f x1) x2 x3 x4
		1 -> Hot4 x1 (f x2) x3 x4
		2 -> Hot4 x1 x2 (f x3) x4
		3 -> Hot4 x1 x2 x3 (f x4)
		n -> hotError 4 "mapAt" n

instance Hot Hot5 5 where
	size _ = 5
	elementAt (Hot5 x1 x2 x3 x4 x5) = \case
		0 -> x1
		1 -> x2
		2 -> x3
		3 -> x4
		4 -> x5
		n -> hotError 5 "elementAt" n
	mapAt f (Hot5 x1 x2 x3 x4 x5) = \case
		0 -> Hot5 (f x1) x2 x3 x4 x5
		1 -> Hot5 x1 (f x2) x3 x4 x5
		2 -> Hot5 x1 x2 (f x3) x4 x5
		3 -> Hot5 x1 x2 x3 (f x4) x5
		4 -> Hot5 x1 x2 x3 x4 (f x5)
		n -> hotError 5 "mapAt" n

instance Hot Hot6 6 where
	size _ = 6
	elementAt (Hot6 x1 x2 x3 x4 x5 x6) = \case
		0 -> x1
		1 -> x2
		2 -> x3
		3 -> x4
		4 -> x5
		5 -> x6
		n -> hotError 6 "elementAt" n
	mapAt f (Hot6 x1 x2 x3 x4 x5 x6) = \case
		0 -> Hot6 (f x1) x2 x3 x4 x5 x6
		1 -> Hot6 x1 (f x2) x3 x4 x5 x6
		2 -> Hot6 x1 x2 (f x3) x4 x5 x6
		3 -> Hot6 x1 x2 x3 (f x4) x5 x6
		4 -> Hot6 x1 x2 x3 x4 (f x5) x6
		5 -> Hot6 x1 x2 x3 x4 x5 (f x6)
		n -> hotError 6 "mapAt" n

instance Hot Hot7 7 where
	size _ = 7
	elementAt (Hot7 x1 x2 x3 x4 x5 x6 x7) = \case
		0 -> x1
		1 -> x2
		2 -> x3
		3 -> x4
		4 -> x5
		5 -> x6
		6 -> x7
		n -> hotError 7 "elementAt" n
	mapAt f (Hot7 x1 x2 x3 x4 x5 x6 x7) = \case
		0 -> Hot7 (f x1) x2 x3 x4 x5 x6 x7
		1 -> Hot7 x1 (f x2) x3 x4 x5 x6 x7
		2 -> Hot7 x1 x2 (f x3) x4 x5 x6 x7
		3 -> Hot7 x1 x2 x3 (f x4) x5 x6 x7
		4 -> Hot7 x1 x2 x3 x4 (f x5) x6 x7
		5 -> Hot7 x1 x2 x3 x4 x5 (f x6) x7
		6 -> Hot7 x1 x2 x3 x4 x5 x6 (f x7)
		n -> hotError 7 "mapAt" n

instance Hot Hot8 8 where
	size _ = 8
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

instance Hot Hot9 9 where
	size _ = 9
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

instance Hot Hot10 10 where
	size _ = 10
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


data Hot1 a = Hot1 !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot2 a = Hot2 !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot3 a = Hot3 !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot4 a = Hot4 !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot5 a = Hot5 !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot6 a = Hot6 !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot7 a = Hot7 !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot8 a = Hot8 !a !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot9 a = Hot9 !a !a !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot10 a = Hot10 !a !a !a !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)
