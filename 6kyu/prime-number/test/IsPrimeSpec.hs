module IsPrimeSpec where

import IsPrime (isPrime)
import Test.Hspec
import Test.QuickCheck

solution :: Integer -> Bool
solution x = x >= 2 && all (\b -> x `rem` b /= 0) [2..(round . sqrt . fromIntegral . abs $ x)]

main = hspec spec

spec =
  describe "isPrime" $ do
    it "should work for some examples" $ do
      isPrime 0        `shouldBe` False
      isPrime 1        `shouldBe` False
      isPrime 2        `shouldBe` True
      isPrime 17       `shouldBe` True
    it "should work for some more examples" $ do
      isPrime 23423527 `shouldBe` True
    it "should work for negative numbers" $ do
      forAll (choose (-1,-(2^32))) $ \x -> do
        isPrime x `shouldBe` False
    it "should work for random examples" $ do
      forAll (choose (1,(2^32))) $ \x -> do
        isPrime x `shouldBe` solution x