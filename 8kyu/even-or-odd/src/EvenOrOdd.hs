module EvenOrOdd where

import Prelude hiding (even, odd)

evenOrOdd :: Integral a => a -> [Char]
evenOrOdd num
    | even num  = "Even"
    | odd  num  = "Odd"
    | otherwise = error "Not an integer"
    where 
        even 0 = True
        even n = odd (n - 1)
        odd 0 = False
        odd n = even (n - 1)
    -- or
        -- even = (== 0) . (`rem` 2)
        -- odd  = not . even