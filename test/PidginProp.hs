module Main (main) where

import Test.QuickCheck

import Data.Hot.Pidgin


main :: IO ()
main = do
  quickCheck prop_merge


prop_merge :: PHot2 Int -> PHot3 Int -> Bool
prop_merge = undefined


instance (Arbitrary a) => Arbitrary (PHot2 a) where
  arbitrary = PHot2 <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (PHot3 a) where
  arbitrary = PHot3 <$> arbitrary <*> arbitrary <*> arbitrary
