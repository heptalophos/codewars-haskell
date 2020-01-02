module CreatePhoneNumber where

createPhoneNumber :: [Int] -> String
createPhoneNumber ds = "(" ++ d 0 ++ d 1 ++ d 2 ++ ") " 
                           ++ d 3 ++ d 4 ++ d 5 ++ "-"  
                           ++ d 6 ++ d 7 ++ d 8 ++ d 9
    where
        d = (!!) . map show $ ds