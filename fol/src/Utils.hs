module Utils(joinS, indent) where

joinS :: [a] -> [[a]] -> [a]
joinS sep []       = []
joinS sep [x]      = x
joinS sep (x : xs) = x ++ sep ++ joinS sep xs

indent :: Int -> String -> String
indent n s = joinS "\n" (map (take n (repeat ' ') ++) (lines s))

