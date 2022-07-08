import Data.List

-- Question 1.
mylen [] = 0
mylen (x:xs) = 1 + mylen xs

-- mylen [] -> 0
-- mylen [1,2] -> 2
-- mylen [1..10] -> 10

-- Question 2.
mylen :: [a] -> Int

-- Question 3.
mean xs = sum xs / fromIntegral (length xs)

-- mean [1,2,3] -> 2.0
-- mean [1,2,8,9] -> 5.0

-- Question 4.
palin xs = xs ++ (reverse xs)

palin2 xs = xs ++ (rev xs [])
  where rev [] ys = ys
        rev (x:xs) ys = rev xs (x:ys)

-- Question 5.
isPalin [] = True
isPalin [_] = True
isPalin (x:xs)
  | x == y = isPalin rest
  | otherwise = False
  where y = head (reverse xs)
        rest = take (length xs - 1) xs

-- Question 6.
sortLists xs = sortBy (\x y -> compare (length x) (length y)) xs

-- Question 7.
-- See intersperse.hs
