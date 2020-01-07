module Expression where

expression :: (Ord a, Num a) => a -> a -> a -> a
expression a b c = 
    let 
        mOverAL = a * (b + c)
        mOverAR = (a + b) * c
        aOverML = a + (b * c)
        aOverMR = (a * b) + c
        justAdd = a + b + c
        justMul = a * b * c
    in
        maximum [mOverAL, mOverAR, aOverML, aOverMR, justAdd, justMul] 