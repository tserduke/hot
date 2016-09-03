module Main (main) where

import Test.Hspec

import Data.Hot


main :: IO ()
main = hspec $ describe "Hot" $ do
  describe "prefix" $ do
    it "5 3" $ prefix (Hot5 1 2 3 4 5) `shouldBe` (Hot3 1 2 3 :: Hot3 Int)
    it "2 1" $ prefix (Hot2 1 2) `shouldBe` (Hot1 1 :: Hot1 Int)
    it "3 3" $ prefix (Hot3 1 2 3) `shouldBe` (Hot3 1 2 3 :: Hot3 Int)
    it "1 1" $ prefix (Hot1 1) `shouldBe` (Hot1 1 :: Hot1 Int)
  describe "suffix" $ do
    it "5 3" $ suffix (Hot5 1 2 3 4 5) `shouldBe` (Hot3 3 4 5 :: Hot3 Int)
    it "2 1" $ suffix (Hot2 1 2) `shouldBe` (Hot1 2 :: Hot1 Int)
    it "3 3" $ suffix (Hot3 1 2 3) `shouldBe` (Hot3 1 2 3 :: Hot3 Int)
    it "1 1" $ suffix (Hot1 1) `shouldBe` (Hot1 1 :: Hot1 Int)
  describe "merge" $ do
    it "3 2" $ merge (Hot3 1 2 3) (Hot2 4 5) `shouldBe` (Hot5 1 2 3 4 5 :: Hot5 Int)
    it "1 2" $ merge (Hot1 2) (Hot2 1 3) `shouldBe` (Hot3 1 2 3 :: Hot3 Int)
    it "4 5" $ merge (Hot4 4 4 4 4) (Hot5 5 5 5 5 5) `shouldBe` (Hot9 4 4 4 4 5 5 5 5 5 :: Hot9 Int)
    it "2 2" $ merge (Hot2 4 5) (Hot2 1 2) `shouldBe` (Hot4 1 2 4 5 :: Hot4 Int)
  describe "sort" $ do
    it "1" $ sort (Hot1 True) `shouldBe` (Hot1 True)
    it "2" $ sort (Hot2 True False) `shouldBe` (Hot2 False True)
    it "3" $ sort (Hot3 5 1 3) `shouldBe` (Hot3 1 3 5 :: Hot3 Int)
    it "7" $ sort (Hot7 4 5 3 7 2 6 1) `shouldBe` (Hot7 1 2 3 4 5 6 7 :: Hot7 Int)
    it "10" $ sort (Hot10 1 9 6 2 5 3 7 4 10 8) `shouldBe` (Hot10 1 2 3 4 5 6 7 8 9 10 :: Hot10 Int)
