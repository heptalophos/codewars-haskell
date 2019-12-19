module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference a b = [x | x <- a, x `notElem` b]