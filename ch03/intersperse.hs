intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [x] = x
intersperse c (x:y:xs) = x ++ [c] ++ (intersperse c (y:xs))
