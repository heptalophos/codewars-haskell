module ParityOutlierSpec (spec, main) where

import ParityOutlier (findOutlier)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  it "example tests" $ do

    findOutlier [2, 4, 0, 100, 4, 11, 2602, 36] `shouldBe` 11
    findOutlier [160, 3, 1719, 19, 11, 13, -21] `shouldBe` 160

    findOutlier [2,6,8,-10,3] `shouldBe` 3
    findOutlier [206847684,1056521,7,17,1901,21104421,7,1,35521,1,7781] `shouldBe` 206847684
    findOutlier [2147483647,0,1] `shouldBe` 0

  it "fixed tests" $ do

    findOutlier [2,6,8,10,3] `shouldBe` 3
    findOutlier [2,6,8,200,700,1,84,10,4] `shouldBe` 1
    findOutlier [17,6,8,10,6,12,24,36] `shouldBe` 17
    findOutlier [2,1,7,17,19,211,7] `shouldBe` 2
    findOutlier [1,1,1,1,1,44,7,7,7,7,7,7,7,7] `shouldBe` 44
    findOutlier [3,3,3,3,3,3,3,3,3,3,3,3,3,3,35,5,5,5,5,5,5,5,5,5,5,7,7,7,7,1000] `shouldBe` 1000
    findOutlier [2,6,8,2,-66,34,-35,66,700,1002,-84,10,4] `shouldBe` -35
    findOutlier [-2147483647,-18,6,-8,-10,6,12,-24,36] `shouldBe` -2147483647
    findOutlier [-20,1,7,17,19,211,7] `shouldBe` -20
    findOutlier [1,1,-1,1,1,-44,7,7,7,7,7,7,7,7] `shouldBe` -44
    findOutlier [1,0,0] `shouldBe` 1
    findOutlier [3,7,-99,81,90211,0,7] `shouldBe` 0
    findOutlier [2,6,8,-10,3] `shouldBe` 3
    findOutlier [1056521,7,17,1901,21104421,7,1,206847684,35521,1,7781] `shouldBe` 206847684
    findOutlier [2147483647,0,1] `shouldBe` 0
    findOutlier [0,2,-1] `shouldBe` -1

  it "random tests" $ do
    forAll genList $ \ (xs,x) -> do
      -- print $ (xs,x)
      findOutlier xs `shouldBe` x

genList :: Gen ([Int],Int)
genList = do
  k <- choose (0,1)
  l <- choose (2,10^4-1)
  i <- choose (0,l)
  xs <- vectorOf i $ (+ k) . (* 2) <$> choose (-10^6,10^6)
  x <- (+ (k+1)) . (* 2) <$> choose (-10^6,10^6)
  ys <- vectorOf (l-i) $ (+ k) . (* 2) <$> choose (-10^6,10^6)
  return ( xs ++ x : ys , x )

main :: IO ()
main = hspec $ describe "FindParityOutlier" spec