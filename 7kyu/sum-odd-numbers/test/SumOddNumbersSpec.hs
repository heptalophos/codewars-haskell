module SumOddNumbersSpec where

import SumOddNumbers (rowSumOddNumbers)
import Test.Hspec
-- import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do 
  describe "rowSumOddNumbers" $ do
    it "should work for some examples" $ do
      rowSumOddNumbers 1 `shouldBe` 1
      rowSumOddNumbers 2 `shouldBe` 8
      rowSumOddNumbers 13 `shouldBe` 2197
      rowSumOddNumbers 19 `shouldBe` 6859
      rowSumOddNumbers 41 `shouldBe` 68921
      rowSumOddNumbers 42 `shouldBe` 74088
    it "should work for some more examples" $ do
      rowSumOddNumbers 74 `shouldBe` 405224
      rowSumOddNumbers 86 `shouldBe` 636056
      rowSumOddNumbers 93 `shouldBe` 804357
      rowSumOddNumbers 101 `shouldBe` 1030301
    it "should work for random input" $ property $
      forAll (choose (1, 10^9)) $ \x -> 
        rowSumOddNumbers x `shouldBe` x ^ 3

main :: IO()
main = hspec spec