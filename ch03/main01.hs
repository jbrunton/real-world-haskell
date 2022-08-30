-- Exercises 1.

-- Question 1.
data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList Nil = []
toList (Cons x xs) = x:(toList xs)

fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

-- Exercises 2.
data Tree a = Node (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)
