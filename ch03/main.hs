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
data Vec2 = Vec2 { getX :: Double, getY :: Double }
  deriving Show

angleBetween :: Vec2 -> Vec2 -> Vec2 -> Double
angleBetween a b c = atan2 (getY v2) (getX v2) - atan2 (getY v1) (getX v1)
  where v1 = Vec2 (getX b - getX a) (getY b - getY a)
        v2 = Vec2 (getX c - getX b) (getY c - getY b)

direction :: Vec2 -> Vec2 -> Vec2 -> Direction
direction a b c
  | alpha == 0 = Main.Straight
  | alpha > 0 = Main.Left
  | alpha < 0 = Main.Right
  where alpha = angleBetween a b c

-- Question 11.

directions :: [Vec2] -> [Direction]
directions (p1:p2:p3:ps) = (direction p1 p2 p3) : (directions (p2:p3:ps))
directions _ = []
