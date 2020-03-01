module RevSeq where 

reverseSeq :: Int -> [Int] 
reverseSeq n = 
    foldl (flip (:)) [] [1..n]
