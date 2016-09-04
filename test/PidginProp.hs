{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}

module Main (main) where

import Test.QuickCheck

import Data.Hot.Pidgin

import Control.Monad (replicateM)
import Data.List (sort)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (IsList, Item)
import GHC.TypeLits (KnownNat, Nat, natVal)


main :: IO ()
main = do
  quickCheck prop_merge2and3


prop_merge2and3 :: OrderedN 2 (PHot2 Int) Int -> OrderedN 3 (PHot3 Int) Int -> Bool
prop_merge2and3 (OrderedN x xs) (OrderedN y ys) = merge2and3 x y == fromList (sort $ xs ++ ys)


data OrderedN (n :: Nat) a e = OrderedN a [e]
  deriving (Show)

instance (KnownNat n, IsList a, e ~ Item a, Arbitrary e, Ord e) => Arbitrary (OrderedN n a e) where
  arbitrary = do
    let n = fromIntegral $ natVal (Proxy :: Proxy n)
    xs <-replicateM n arbitrary
    let xs' = sort xs
    return $ OrderedN (fromList xs') xs'
