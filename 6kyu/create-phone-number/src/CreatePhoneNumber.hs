module CreatePhoneNumber where

createPhoneNumber :: [Int] -> String
createPhoneNumber digits = "(" ++ area ++ ") " ++ 
                           exchange ++ "-" ++ subscriber 
    where
        area       = digitAt 0 ++ digitAt 1 ++ digitAt 2
        exchange   = digitAt 3 ++ digitAt 4 ++ digitAt 5
        subscriber = digitAt 6 ++ digitAt 7 ++ 
                     digitAt 8 ++ digitAt 9 
        digitAt = ((!!) . map show) digits