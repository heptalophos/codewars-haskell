module Solution where

solution :: String -> String
solution = foldl (\acc x -> x : acc) [] 