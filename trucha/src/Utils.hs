
module Utils(thenFail, splitBy, joinS, dropLast, filterSplit, indent) where 

thenFail :: Bool -> String -> Either String ()
thenFail b msg = if b
                  then Left msg
                  else Right ()

splitBy :: (Char -> Bool) -> String -> [String]
splitBy p ""            = [""]
splitBy p (c : s) | p c = "" : splitBy p s
splitBy p (c : s)       = let (r : rs) = splitBy p s
                           in (c : r) : rs

joinS :: [a] -> [[a]] -> [a]
joinS sep []       = []
joinS sep [x]      = x
joinS sep (x : xs) = x ++ sep ++ joinS sep xs

dropLast :: Int -> [a] -> [a]
dropLast n list = reverse (drop n (reverse list))

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit p [] = ([], [])
filterSplit p (x : xs) =
  let (ys, zs) = filterSplit p xs in
    if p x
     then (x : ys, zs)
     else (ys, x : zs)

indent :: Integer -> String -> String
indent n str = joinS "\n" (map (replicate (fromIntegral n) ' ' ++) (lines str))
