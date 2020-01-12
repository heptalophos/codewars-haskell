module SolutionTest where

import Solution
import Test.Hspec
import Test.QuickCheck

main = hspec $ do
  it "Sample tests" $ do
    solution "world" `shouldBe` "dlrow"
    solution "hello" `shouldBe` "olleh"
    solution "" `shouldBe` ""
    solution "h" `shouldBe` "h"
    solution "Codewars rules" `shouldBe` "selur srawedoC"
  it "Random tests" $ do
    property $ \s -> solution s == reverse s