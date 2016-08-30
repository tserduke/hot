{-# LANGUAGE DeriveDataTypeable, FunctionalDependencies, KindSignatures #-}

module Data.Hot.Base where

import Data.Data (Data, Typeable)
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
	mapAt :: (a -> a) -> t a -> Int -> t a


instance Hot Hot1 1 where
	size _ = 1
	mapAt f (T1 x1) = \case
		0 -> T1 (f x1)
		n -> error $ "Hot1 mapAt " ++ show n

instance Hot Hot2 2 where
	size _ = 2
	mapAt f (T2 x1 x2) = \case
		0 -> T2 (f x1) x2
		1 -> T2 x1 (f x2)
		n -> error $ "Hot2 mapAt " ++ show n

instance Hot Hot3 3 where
	size _ = 3
	mapAt f (T3 x1 x2 x3) = \case
		0 -> T3 (f x1) x2 x3
		1 -> T3 x1 (f x2) x3
		2 -> T3 x1 x2 (f x3)
		n -> error $ "Hot3 mapAt " ++ show n

instance Hot Hot4 4 where
	size _ = 4
	mapAt f (T4 x1 x2 x3 x4) = \case
		0 -> T4 (f x1) x2 x3 x4
		1 -> T4 x1 (f x2) x3 x4
		2 -> T4 x1 x2 (f x3) x4
		3 -> T4 x1 x2 x3 (f x4)
		n -> error $ "Hot4 mapAt " ++ show n

instance Hot Hot5 5 where
	size _ = 5
	mapAt f (T5 x1 x2 x3 x4 x5) = \case
		0 -> T5 (f x1) x2 x3 x4 x5
		1 -> T5 x1 (f x2) x3 x4 x5
		2 -> T5 x1 x2 (f x3) x4 x5
		3 -> T5 x1 x2 x3 (f x4) x5
		4 -> T5 x1 x2 x3 x4 (f x5)
		n -> error $ "Hot5 mapAt " ++ show n

instance Hot Hot6 6 where
	size _ = 6
	mapAt f (T6 x1 x2 x3 x4 x5 x6) = \case
		0 -> T6 (f x1) x2 x3 x4 x5 x6
		1 -> T6 x1 (f x2) x3 x4 x5 x6
		2 -> T6 x1 x2 (f x3) x4 x5 x6
		3 -> T6 x1 x2 x3 (f x4) x5 x6
		4 -> T6 x1 x2 x3 x4 (f x5) x6
		5 -> T6 x1 x2 x3 x4 x5 (f x6)
		n -> error $ "Hot6 mapAt " ++ show n

instance Hot Hot7 7 where
	size _ = 7
	mapAt f (T7 x1 x2 x3 x4 x5 x6 x7) = \case
		0 -> T7 (f x1) x2 x3 x4 x5 x6 x7
		1 -> T7 x1 (f x2) x3 x4 x5 x6 x7
		2 -> T7 x1 x2 (f x3) x4 x5 x6 x7
		3 -> T7 x1 x2 x3 (f x4) x5 x6 x7
		4 -> T7 x1 x2 x3 x4 (f x5) x6 x7
		5 -> T7 x1 x2 x3 x4 x5 (f x6) x7
		6 -> T7 x1 x2 x3 x4 x5 x6 (f x7)
		n -> error $ "Hot7 mapAt " ++ show n

instance Hot Hot8 8 where
	size _ = 8
	mapAt f (T8 x1 x2 x3 x4 x5 x6 x7 x8) = \case
		0 -> T8 (f x1) x2 x3 x4 x5 x6 x7 x8
		1 -> T8 x1 (f x2) x3 x4 x5 x6 x7 x8
		2 -> T8 x1 x2 (f x3) x4 x5 x6 x7 x8
		3 -> T8 x1 x2 x3 (f x4) x5 x6 x7 x8
		4 -> T8 x1 x2 x3 x4 (f x5) x6 x7 x8
		5 -> T8 x1 x2 x3 x4 x5 (f x6) x7 x8
		6 -> T8 x1 x2 x3 x4 x5 x6 (f x7) x8
		7 -> T8 x1 x2 x3 x4 x5 x6 x7 (f x8)
		n -> error $ "Hot8 mapAt " ++ show n

instance Hot Hot9 9 where
	size _ = 9
	mapAt f (T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = \case
		0 -> T9 (f x1) x2 x3 x4 x5 x6 x7 x8 x9
		1 -> T9 x1 (f x2) x3 x4 x5 x6 x7 x8 x9
		2 -> T9 x1 x2 (f x3) x4 x5 x6 x7 x8 x9
		3 -> T9 x1 x2 x3 (f x4) x5 x6 x7 x8 x9
		4 -> T9 x1 x2 x3 x4 (f x5) x6 x7 x8 x9
		5 -> T9 x1 x2 x3 x4 x5 (f x6) x7 x8 x9
		6 -> T9 x1 x2 x3 x4 x5 x6 (f x7) x8 x9
		7 -> T9 x1 x2 x3 x4 x5 x6 x7 (f x8) x9
		8 -> T9 x1 x2 x3 x4 x5 x6 x7 x8 (f x9)
		n -> error $ "Hot9 mapAt " ++ show n

instance Hot Hot10 10 where
	size _ = 10
	mapAt f (T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = \case
		0 -> T10 (f x1) x2 x3 x4 x5 x6 x7 x8 x9 x10
		1 -> T10 x1 (f x2) x3 x4 x5 x6 x7 x8 x9 x10
		2 -> T10 x1 x2 (f x3) x4 x5 x6 x7 x8 x9 x10
		3 -> T10 x1 x2 x3 (f x4) x5 x6 x7 x8 x9 x10
		4 -> T10 x1 x2 x3 x4 (f x5) x6 x7 x8 x9 x10
		5 -> T10 x1 x2 x3 x4 x5 (f x6) x7 x8 x9 x10
		6 -> T10 x1 x2 x3 x4 x5 x6 (f x7) x8 x9 x10
		7 -> T10 x1 x2 x3 x4 x5 x6 x7 (f x8) x9 x10
		8 -> T10 x1 x2 x3 x4 x5 x6 x7 x8 (f x9) x10
		9 -> T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 (f x10)
		n -> error $ "Hot10 mapAt " ++ show n


data Hot1 a = T1 !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot2 a = T2 !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot3 a = T3 !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot4 a = T4 !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot5 a = T5 !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot6 a = T6 !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot7 a = T7 !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot8 a = T8 !a !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot9 a = T9 !a !a !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)

data Hot10 a = T10 !a !a !a !a !a !a !a !a !a !a
	deriving (Eq, Ord, Read, Show, Data, Typeable)
