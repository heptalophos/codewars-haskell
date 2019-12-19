import Difference (difference)

import Test.Hspec (hspec, describe, it)

main = hspec $ do
  describe "difference" $ do  
    it "example with zero occurences" $ do 
      difference [1,2,2,3] [] == [1,2,2,3]   
    it "example with one occurence" $ do
      difference [1,2,3] [2] == [1,3]
    it "example with two occurences" $ do
      difference [1,2,2,3] [2] == [1,3]