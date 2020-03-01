module ParityOutlier (findOutlier) where

import Data.Maybe
import Data.List 

-- As in every array there are at least 3 elements with 
-- only one of opposite parity than the rest of them,
-- get the sum of the 1st 3 {element modulo 2}s.
-- If the sum is 0 or 1 the array contains mostly evens,
-- and if it is 2 or 3 it contains mostly odds.

findOutlier :: [Int] -> Int 
findOutlier xs =
    case compare sumFirstThreeMod2s 2 of 
        LT -> fromMaybe 0 $ find odd xs
        _  -> fromMaybe 0 $ find even xs
    where 
        sumFirstThreeMod2s = 
            sum $ map (flip mod 2) $ take 3 xs