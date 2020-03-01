module IsPrime where

isPrime :: Integer -> Bool
isPrime x 
    | x <= 1    = 
        False
    | otherwise = 
        not $ elem 0 (map (mod x) [2..(intsqrt x)])
    where
        intsqrt = floor . sqrt . fromIntegral