module RevSeqTest where

import RevSeq
import Test.Hspec
import Test.QuickCheck

main = hspec $ do
  describe "Reverse Sequence" $ do
    it "Basic tests" $ do
      reverseSeq 5 `shouldBe` [5,4,3,2,1]
      reverseSeq 6 `shouldBe` [6,5,4,3,2,1]

    it "More tests" $ do
      reverseSeq 10 `shouldBe` [10,9..1]
      reverseSeq 100 `shouldBe` [100,99..1]
      reverseSeq 1000 `shouldBe` [1000,999..1]
      
    it "Random tests" $ do
      forAll (choose (1, 10000)) $ \a ->      
          reverseSeq a `shouldBe` solver98 a
          
solver98 :: Int -> [Int] 
solver98 n = [n, n-1..1]