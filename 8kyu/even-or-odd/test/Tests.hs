import EvenOrOdd
import Test.Hspec

main = hspec $ do
  describe "Examples" $ do
    it "Evens: 2 0 12 20002" $ do
      evenOrOdd 2 `shouldBe` "Even"
      evenOrOdd 0 `shouldBe` "Even"
      evenOrOdd 12 `shouldBe` "Even"
      evenOrOdd 20002 `shouldBe` "Even"
    it "Odds: 7 1 23 32849" $ do
      evenOrOdd 7 `shouldBe` "Odd"
      evenOrOdd 1 `shouldBe` "Odd"
      evenOrOdd 23 `shouldBe` "Odd"
      evenOrOdd 32849 `shouldBe` "Odd"