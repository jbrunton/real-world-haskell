import Data.List

-- Exercises 1.

-- Question 1.
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- Question 2.
toList Nil = []
toList (Cons x xs) = x:(toList xs)

fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

-- Exercises 2.

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
  deriving Show

-- Question 10.
type Vec2 = (Double, Double)

dotp :: Vec2 -> Vec2 -> Double
dotp (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

size :: Vec2 -> Double
size (x, y) = sqrt (x**2 + y**2)

direction :: Vec2 -> Vec2 -> Vec2 -> Direction
direction (ax, ay) (bx, by) (cx, cy) = dir alpha
  where alpha = atan2 (snd v2) (fst v2) - atan2 (snd v1) (fst v1)
        dir 0 = Main.Straight
        dir a | a < 0 = Main.Left
        dir a | a > 0 = Main.Right
        v1 = (bx - ax, by - ay)
        v2 = (cx - bx, cy - by)
