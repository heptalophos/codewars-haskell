module ExpressionSpec where
-- Tests can be written using Hspec http://hspec.github.io/

import Test.Hspec
import Expression

import Test.QuickCheck

-- `spec` of type `Spec` must exist
spec :: Spec
spec = do
    describe "finds the max" $ do
        it "some simple tests" $ do
            (expression 1 1 1) `shouldBe` (3)
            (expression 2 3 2) `shouldBe` (12)
            (expression 1 2 3) `shouldBe` (9)
            (expression 4 1 1) `shouldBe` (8)
        it "handles random tests" $ property $
          forAll genArgs $ \(a,b,c) -> expression a b c == maximum [a+b+c,(a+b)*c,a*(b+c),a*b*c]
          
genArgs :: Gen (Int,Int,Int)
genArgs = do
  a <- choose(1,10)
  b <- choose(1,10)
  c <- choose(1,10)
  return (a,b,c)

main :: IO ()
main = hspec spec