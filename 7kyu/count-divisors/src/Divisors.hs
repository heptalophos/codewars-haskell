module Divisors where

divisors :: Integral a => a -> Int
divisors x = 
    sum [0 ^ mod x n | n <- [1..x]]