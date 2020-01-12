module Solution where

solution :: String -> String
solution = foldl (flip (:)) [] 