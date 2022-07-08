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

-- Question 8.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node x t1 t2) = 1 + max (height t1) (height t2)

-- Question 9.
data Direction = Left | Right | Straight

-- Question 10.
-- data Point = Point { ptX :: Double, ptY :: Double }
-- direction :: (Point, Point, Point) -> Direction
-- direction ((ax, ay), (bx, by), (cx, cy)) = dir alpha
--   where alpha = acos (dotp / (sizeV1 * sizeV2))
--         dir 0 = Main.Straight
--         dir a | a < 0 = Main.Left
--         dir a | a > 0 = Main.Right
--         where dotp = (ptX v1 * ptX v2 + ptY v1 * ptY v2)
--               sizeV1 = sqrt ((ptX v1)**2 + (ptY v1)**2)
--               sizeV2 = sqrt ((ptX v2)**2 + (ptY v2)**2)
--               where v1 = (bx - ax, by - ay)
--                     v2 = (cx - bx, cy - by)
        