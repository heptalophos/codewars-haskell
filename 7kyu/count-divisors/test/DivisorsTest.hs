module DivisorsTest where

import Divisors (divisors)
import Test.Hspec
import Test.QuickCheck

solution :: Integral a => a -> Int
solution x = length . filter ((==0).(x `rem`)) $ [1..x]

main = hspec $ do
  describe "divisors" $ do
    it "should return 1 for 1"  $ divisors 1  `shouldBe` 1
    it "should return 3 for 4"  $ divisors 4  `shouldBe` 3
    it "should return 2 for 5"  $ divisors 5  `shouldBe` 2
    it "should return 6 for 12" $ divisors 12 `shouldBe` 6
    it "should return 8 for 30" $ divisors 30 `shouldBe` 8
    it "should return 3 for 25" $ divisors 25 `shouldBe` 3
    it "should work for random inputs (Integer)" $ do
      property $ forAll (suchThat arbitrary (>0)) $ \x ->
        divisors x `shouldBe` solution (x :: Integer)
    it "should work for random inputs (Int)" $ do
      property $ forAll (suchThat arbitrary (>0)) $ \x ->
        divisors x `shouldBe` solution (x :: Int)