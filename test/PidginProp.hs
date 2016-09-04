{-# LANGUAGE DataKinds, KindSignatures #-}

module Main (main) where

import Test.QuickCheck

import Data.Hot.Pidgin

import Control.Monad (replicateM)
import Data.List.Extra (merge, sort)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Nat, natVal)


main :: IO ()
main = do
  quickCheck prop_merge2and3


prop_merge2and3 :: OrderedListN 2 Int -> OrderedListN 3 Int -> Bool
prop_merge2and3 (OrderedN x) (OrderedN y) =
  merge2and3 (fromList x) (fromList y) == fromList (merge x y) where


newtype OrderedListN (n :: Nat) a = OrderedN [a]
  deriving (Show)

instance (KnownNat n, Arbitrary a, Ord a) => Arbitrary (OrderedListN n a) where
  arbitrary = OrderedN . sort <$> replicateM n arbitrary where
    n = fromIntegral $ natVal (Proxy :: Proxy n)
