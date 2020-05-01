module RomanNumeralsTest where

import Test.Hspec
import Test.QuickCheck
import RomanNumerals (solution)

numerals :: [(String, Integer)]
numerals = zip ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I" ]
               [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

referenceSolution :: Integer -> String
referenceSolution 0 = ""
referenceSolution n = k ++ referenceSolution (n - v)
                      where (k, v) = head $ filter ((<=n).snd) numerals

main = hspec $ do
  describe "Some tests" $ do
    it "should translate 1 to I" $ solution 1 `shouldBe` "I"
    it "should translate 4 to IV" $ solution 4 `shouldBe` "IV"
    it "should translate 6 to VI" $ solution 6 `shouldBe` "VI"
    it "should translate 14 to XIV" $ solution 14 `shouldBe` "XIV"
    it "should translate 21 to XXI" $ solution 21 `shouldBe` "XXI"
    it "should translate 89 to LXXXIX" $ solution 89 `shouldBe` "LXXXIX"
    it "should translate 91 to XCI" $ solution 91 `shouldBe` "XCI"
    it "should translate 984 to CMLXXXIV" $ solution 984 `shouldBe` "CMLXXXIV"
    it "should translate 1000 to M" $ solution 1889 `shouldBe` "MDCCCLXXXIX"
    it "should translate 1889 to MDCCCLXXXIX" $ solution 1889 `shouldBe` "MDCCCLXXXIX"
    it "should translate 1989 to MCMLXXXIX" $ solution 1989 `shouldBe` "MCMLXXXIX"
  describe "Randomized tests" $ do
    it "should work for random examples" $ forAll (choose (1,3888)) $
        \x -> solution (abs x) == referenceSolution (abs x)