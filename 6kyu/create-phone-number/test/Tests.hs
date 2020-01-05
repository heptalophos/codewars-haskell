-- module KataSpec (main) where

import CreatePhoneNumber
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Basic tests" $ do
    createPhoneNumber [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] `shouldBe` "(000) 000-0000"
    createPhoneNumber [5, 5, 5, 6, 4, 7, 6, 4, 2, 7] `shouldBe` "(555) 647-6427"
    createPhoneNumber [0, 2, 4, 6, 8, 9, 7, 5, 3, 1] `shouldBe` "(024) 689-7531"
    createPhoneNumber [0, 4, 4, 7, 0, 0, 7, 0, 7, 0] `shouldBe` "(044) 700-7070"
  it "Random tests" $ property $
    forAll (vectorOf 10 (choose (0, 9))) $ \ a ->
      createPhoneNumber a `shouldBe` sol a
          
sol :: [Int] -> String
sol input = "(" ++ digits 0 3 ++ ") " ++ digits 3 3 ++ "-" ++ digits 6 4
  where digits start count = concat . map show . take count . drop start $ input    